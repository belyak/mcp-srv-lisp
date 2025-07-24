;;;; OO Initialization and Integration
;;;; This file initializes all OO improvements and provides integration points

(in-package :mcp-server)

;;; Initialize all OO systems
(defun initialize-oo-systems ()
  "Initialize all OO improvements"
  ;; Initialize the registry with default components
  (initialize-default-registry)
  ;; Initialize the command tools system
  (register-default-tools)
  ;; Set up factory type mappings
  (initialize-factories)
  (format *error-output* "[MCP-LISP] OO systems initialized~%"))

(defun initialize-default-registry ()
  "Initialize the registry with default tools, prompts, and resources"
  ;; Load and register tools from template
  (let ((tools-template (coerce (mcp-server.templates:load-tools-template) 'list)))
    (dolist (tool-data tools-template)
      (let* ((name (gethash "name" tool-data))
             (tool (create-tool-from-template tool-data)))
        (register-component *global-registry* :tool name tool))))
  
  ;; Load and register prompts from template
  (let ((prompts-template (coerce (mcp-server.templates:load-prompts-template) 'list)))
    (dolist (prompt-data prompts-template)
      (let* ((name (gethash "name" prompt-data))
             (prompt (create-prompt-from-template prompt-data)))
        (register-component *global-registry* :prompt name prompt))))
  
  ;; Load and register resources from template
  (let ((resources-template (coerce (mcp-server.templates:load-resources-template) 'list)))
    (dolist (resource-data resources-template)
      (let* ((uri (gethash "uri" resource-data))
             (resource (create-resource-from-template resource-data)))
        (register-component *global-registry* :resource uri resource)))))

(defun initialize-factories ()
  "Set up factory type mappings"
  ;; Register additional tool types in the factory
  (register-type *tool-factory* "time_tool" 'time-tool)
  (register-type *tool-factory* "calculator_tool" 'calculator-tool))

;;; Enhanced handler functions that use OO improvements

(defun handle-tools-list-enhanced ()
  "Enhanced tools list handler using registry"
  (let ((tools (get-available-tools-from-registry)))
    (make-instance 'list-tools-result
                   :tools (coerce tools 'vector))))

(defun handle-tool-call-enhanced (tool-name params)
  "Enhanced tool call handler using registry and command pattern"
  (let ((tool (find-tool-in-registry tool-name)))
    (if tool
        (execute-tool tool params)
        (error 'tool-not-found-error :tool-name tool-name))))

(defun handle-prompts-list-enhanced ()
  "Enhanced prompts list handler using registry"
  (let ((prompts (get-available-prompts-from-registry)))
    (make-instance 'list-prompts-result
                   :prompts (coerce prompts 'vector))))

(defun handle-resources-list-enhanced ()
  "Enhanced resources list handler using registry"
  (let ((resources (get-available-resources-from-registry)))
    (make-instance 'list-resources-result
                   :resources (coerce resources 'vector))))

;;; Aspect-oriented features using method combinations

(defmethod handle-request :before ((request mcp-request))
  "Log all incoming requests"
  (when *debug-mode*
    (log-message (format nil "Handling request: ~A" (request-method request)))))

(defmethod handle-request :after ((request mcp-request))
  "Log completed requests"
  (when *debug-mode*
    (log-message (format nil "Completed request: ~A" (request-method request)))))

(defmethod handle-request :around ((request mcp-request))
  "Add timing information to requests"
  (let ((start-time (get-internal-real-time)))
    (prog1 (call-next-method)
      (when *debug-mode*
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (log-message (format nil "Request ~A took ~,3f seconds"
                              (request-method request)
                              elapsed)))))))

;;; Plugin system integration

(defclass default-tools-plugin (mcp-plugin)
  ()
  (:default-initargs
   :name "default-tools"
   :version "1.0.0"
   :description "Default MCP tools plugin"))

(defmethod initialize-plugin ((plugin default-tools-plugin) registry)
  "Initialize default tools plugin"
  (register-tool registry (make-instance 'time-tool))
  (register-tool registry (make-instance 'calculator-tool)))

;;; Migration utilities

(defun migrate-to-oo-system ()
  "Migrate existing system to use OO improvements"
  ;; This function can be called to gradually migrate from old patterns
  (initialize-oo-systems)
  (format *error-output* "[MCP-LISP] Migration to OO system completed~%"))

;;; Development and debugging utilities

(defun show-oo-stats ()
  "Show statistics about the OO system"
  (let ((stats (registry-stats)))
    (format t "OO System Statistics:~%")
    (format t "  Tools: ~A~%" (getf stats :tools))
    (format t "  Prompts: ~A~%" (getf stats :prompts))
    (format t "  Resources: ~A~%" (getf stats :resources))
    (format t "  Plugins: ~A~%" (getf stats :plugins))))

(defun test-oo-patterns ()
  "Test all OO patterns are working correctly"
  (format t "Testing OO patterns...~%")
  
  ;; Test protocol-based dispatch
  (let ((request (make-instance 'ping-request :method "ping" :id "test-1")))
    (handle-request request)
    (format t "✓ Protocol-based dispatch working~%"))
  
  ;; Test registry
  (let ((tool (find-component *global-registry* :tool "get_current_time_in_city")))
    (if tool
        (format t "✓ Registry system working~%")
        (format t "✗ Registry system failed~%")))
  
  ;; Test factory
  (let ((test-data (alexandria:alist-hash-table 
                    '(("name" . "test-tool")
                      ("description" . "Test tool")
                      ("inputSchema" . nil))
                    :test 'equal)))
    (handler-case
        (progn
          (create-from-json 'tool test-data)
          (format t "✓ Factory pattern working~%"))
      (error (e)
        (format t "✗ Factory pattern failed: ~A~%" e))))
  
  ;; Test JSON mixins
  (let ((tool (make-instance 'tool-v3 
                            :name "test"
                            :description "test tool")))
    (handler-case
        (progn
          (encode-to-json tool)
          (format t "✓ JSON mixins working~%"))
      (error (e)
        (format t "✗ JSON mixins failed: ~A~%" e))))
  
  (format t "OO pattern testing completed~%"))

;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (unless (boundp '*oo-initialized*)
    (defvar *oo-initialized* nil)
    (initialize-oo-systems)
    (setf *oo-initialized* t)))