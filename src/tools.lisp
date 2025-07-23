;; ====================================================================
;; Tools Implementation (src/tools.lisp)
;; ====================================================================

(defpackage #:mcp-server.tools
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:tool
                #:tool-input-schema
                #:tool-input-schema-property
                #:list-tools-result
                #:call-tool-result
                #:call-tool-result-content)
  (:import-from #:mcp-server.templates
                #:load-tools-template)
  (:import-from #:local-time
                #:now
                #:format-rfc1123-timestring)
  (:export #:handle-tools-list
           #:handle-tool-call
           #:get-available-tools))

(in-package #:mcp-server.tools)

(defvar *tools-cache* nil
  "Cache for loaded tools")

(defun load-tools ()
  "Load tools from template data"
  (unless *tools-cache*
    (setf *tools-cache* (load-tools-template)))
  *tools-cache*)

(defun convert-tool-from-json (tool-json)
  "Convert JSON tool definition to tool object"
  (let ((properties (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (setf (gethash key properties)
                     (make-instance 'tool-input-schema-property
                                    :type-name (gethash "type" value)
                                    :description (gethash "description" value))))
             (gethash "properties" (gethash "inputSchema" tool-json)))
    
    (make-instance 'tool
                   :name (gethash "name" tool-json)
                   :description (gethash "description" tool-json)
                   :input-schema (make-instance 'tool-input-schema
                                                :type-name (gethash "type" (gethash "inputSchema" tool-json))
                                                :properties properties
                                                :required (coerce (gethash "required" (gethash "inputSchema" tool-json)) 'list)))))

(defun get-available-tools ()
  "Get list of available tools"
  (map 'list #'convert-tool-from-json (load-tools)))

(defun handle-tools-list (request)
  "Handle tools/list request"
  (declare (ignore request))
  (make-instance 'list-tools-result
                 :tools (get-available-tools)))

(defun handle-get-current-time (params)
  "Handle get_current_time_in_city tool call"
  (let* ((city (gethash "city" params "Unknown"))
         (current-time (local-time:now))
         (time-string (local-time:format-rfc1123-timestring nil current-time))
         (result-text (format nil "Now: ~A!" time-string)))
    
    (make-instance 'call-tool-result
                   :content (list (make-instance 'call-tool-result-content
                                                 :type-name "text"
                                                 :text result-text))
                   :is-error nil)))

(defun handle-tool-call (method params)
  "Handle tool call requests"
  (cond
    ((string= method "get_current_time_in_city")
     (handle-get-current-time params))
    (t
     (error "Unknown tool: ~A" method))))

