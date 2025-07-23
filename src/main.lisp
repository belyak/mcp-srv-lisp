;; ====================================================================
;; Main Server Implementation (src/main.lisp)
;; ====================================================================

(defpackage #:mcp-server.main
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:parse-from-json
                #:encode-to-json
                #:json-rpc-request
                #:json-rpc-notification
                #:make-json-rpc-response
                #:make-json-rpc-error)
  (:import-from #:mcp-server.utilities
                #:handle-initialize
                #:handle-ping
                #:handle-connect
                #:handle-disconnect
                #:handle-execute-line
                #:handle-logging-set-level
                #:handle-roots-list
                #:handle-cancelled-notification
                #:graceful-shutdown)
  (:import-from #:mcp-server.tools
                #:handle-tools-list
                #:handle-tool-call)
  (:import-from #:mcp-server.prompts
                #:handle-prompts-list
                #:handle-prompts-get)
  (:import-from #:mcp-server.resources
                #:handle-resources-list
                #:handle-resource-read)
  (:import-from #:unix-opts
                #:define-opts
                #:get-opts)
  (:import-from #:trivial-signal
                #:signal-handler-bind)
  (:export #:main))

(in-package #:mcp-server.main)

;; Command line options
(unix-opts:define-opts
  (:name :tools
   :description "list tools"
   :long "tools")
   
  (:name :prompts
   :description "list prompts"
   :long "prompts")
   
  (:name :resources
   :description "list resources"
   :long "resources")
   
  (:name :mcp
   :description "start MCP server"
   :long "mcp")
   
  (:name :stdio
   :description "start MCP server (alias for --mcp)"
   :long "stdio")
   
  (:name :json
   :description "output as json-rpc format"
   :long "json")
   
  (:name :help
   :description "print help text"
   :short #\h
   :long "help"))

(defvar *debug-mode* nil
  "Enable debug mode")

(defvar *log-file* "/tmp/mcp.jsonl"
  "Log file path")

(defun log-message (message)
  "Log message to file"
  (with-open-file (stream *log-file* 
                          :direction :output 
                          :if-exists :append 
                          :if-does-not-exist :create)
    (format stream "~A~%" message)))

(defun dispatch-request (method params id)
  "Dispatch JSON-RPC request to appropriate handler"
  (handler-case
      (let ((result 
             (cond
               ((string= method "initialize")
                (handle-initialize params))
               ((string= method "ping")
                (handle-ping params))
               ((string= method "tools/list")
                (handle-tools-list params))
               ((string= method "tools/call")
                (let ((tool-name (gethash "name" params))
                      (tool-args (gethash "arguments" params)))
                  (handle-tool-call tool-name tool-args)))
               ((string= method "prompts/list")
                (handle-prompts-list params))
               ((string= method "prompts/get")
                (handle-prompts-get params))
               ((string= method "resources/list")
                (handle-resources-list params))
               ((string= method "resources/read")
                (handle-resource-read params))
               ((string= method "connect")
                (handle-connect params))
               ((string= method "disconnect")
                (handle-disconnect params))
               ((string= method "execute_line")
                (handle-execute-line params))
               ((string= method "logging/setLevel")
                (handle-logging-set-level params))
               ((string= method "roots/list")
                (handle-roots-list params))
               ;; Direct tool calls
               ((string= method "get_current_time_in_city")
                (handle-tool-call method params))
               (t
                (error "Method not found: ~A" method)))))
        
        (make-json-rpc-response id result))
    
    (error (e)
      (make-json-rpc-error id -32603 (format nil "Internal error: ~A" e)))))

(defun handle-notification (method params)
  "Handle JSON-RPC notification"
  (cond
    ((string= method "notifications/initialized")
     (format *error-output* "[MCP-LISP] Client initialized~%"))
    ((string= method "notifications/cancelled")
     (handle-cancelled-notification params))
    (t
     (format *error-output* "[MCP-LISP] Unknown notification: ~A~%" method))))

(defun process-json-line (line)
  "Process a single JSON-RPC line"
  (when (and line (> (length (string-trim '(#\Space #\Tab #\Newline) line)) 0))
    (log-message line)
    
    (handler-case
        (let ((parsed (yason:parse line)))
          (cond
            ;; Notification (no id field)
            ((and (hash-table-p parsed)
                  (gethash "method" parsed)
                  (not (gethash "id" parsed)))
             (handle-notification (gethash "method" parsed)
                                  (gethash "params" parsed)))
            
            ;; Request (has id field)
            ((and (hash-table-p parsed)
                  (gethash "method" parsed)
                  (gethash "id" parsed))
             (let* ((method (gethash "method" parsed))
                    (params (gethash "params" parsed))
                    (id (gethash "id" parsed))
                    (response (dispatch-request method params id)))
               
               (when response
                 (let ((response-json (encode-to-json response)))
                   (log-message response-json)
                   (format t "~A~%" response-json)
                   (force-output)))))
            
            (t
             (format *error-output* "[MCP-LISP] Invalid JSON-RPC message~%"))))
      
      (error (e)
        (format *error-output* "[MCP-LISP] Error processing message: ~A~%" e)))))

(defun start-server ()
  "Start the MCP server"
  (format *error-output* "[MCP-LISP] MCP server starting (PID: ~A)~%" (sb-posix:getpid))
  
  ;; Set up signal handlers
  (trivial-signal:signal-handler-bind
      ((:int (lambda (signal)
               (declare (ignore signal))
               (format *error-output* "[MCP-LISP] Received SIGINT, shutting down~%")
               (graceful-shutdown)
               (sb-ext:exit :code 0)))
       (:term (lambda (signal)
                (declare (ignore signal))
                (format *error-output* "[MCP-LISP] Received SIGTERM, shutting down~%")
                (graceful-shutdown)
                (sb-ext:exit :code 0))))
    
    ;; Main server loop
    (loop for line = (read-line *standard-input* nil nil)
          while line
          do (process-json-line line)))
  
  (format *error-output* "[MCP-LISP] MCP server exiting~%"))

(defun display-info (options)
  "Display information about available tools, prompts, resources"
  (let ((show-tools (getf options :tools))
        (show-prompts (getf options :prompts))
        (show-resources (getf options :resources))
        (json-output (getf options :json)))
    
    (unless (or show-tools show-prompts show-resources)
      (format t "Please use --help to see available options~%")
      (return-from display-info))
    
    (if json-output
        ;; JSON output
        (progn
          (when show-tools
            (let ((tools (handle-tools-list nil)))
              (format t "~A~%" (encode-to-json tools))))
          (when show-prompts
            (let ((prompts (handle-prompts-list nil)))
              (format t "~A~%" (encode-to-json prompts))))
          (when show-resources
            (let ((resources (handle-resources-list nil)))
              (format t "~A~%" (encode-to-json resources)))))
        
        ;; Text output
        (progn
          (when show-tools
            (format t "tools:~%- get_current_time_in_city: get current time in city~%~%"))
          (when show-prompts
            (format t "prompts:~%- current_time: get current time in city~%~%"))
          (when show-resources
            (format t "resources:~%- sqlite: file:///path/to/sqlite.db~%~%"))))))

(defun main (&optional args)
  "Main entry point"
  (handler-case
      (multiple-value-bind (options free-args)
          (unix-opts:get-opts (or args (uiop:command-line-arguments)))
        
        (declare (ignore free-args))
        
        (when (getf options :help)
          (unix-opts:describe)
          (return-from main))
        
        ;; Check for stdio alias
        (when (getf options :stdio)
          (setf (getf options :mcp) t))
        
        (if (getf options :mcp)
            (start-server)
            (display-info options)))
    
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:exit :code 1))))

;; Entry point for the binary
(defun mcp-server:main ()
  (main))

