;; ====================================================================
;; Tools Implementation (src/tools.lisp)
;; ====================================================================

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
                                    :type (gethash "type" value)
                                    :description (gethash "description" value))))
             (gethash "properties" (gethash "inputSchema" tool-json)))
    
    (make-instance 'tool
                   :name (gethash "name" tool-json)
                   :description (gethash "description" tool-json)
                   :input-schema (make-instance 'tool-input-schema
                                                :type (gethash "type" (gethash "inputSchema" tool-json))
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
         (result-text (format nil "Now: ~A (in ~A)!" time-string city)))
    
    (make-instance 'call-tool-result
                   :content (list (make-instance 'call-tool-result-content
                                                 :type "text"
                                                 :text result-text))
                   :is-error nil)))

(defun handle-tool-call (method params)
  "Handle tool call requests"
  (cond
    ((string= method "get_current_time_in_city")
     (handle-get-current-time params))
    (t
     (error "Unknown tool: ~A" method))))

