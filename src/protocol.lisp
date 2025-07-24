;;;; Protocol-based request handling for MCP server
;;;; This implements the OOP improvement #1: Protocol-Based Request Handling

(in-package :mcp-server)


;;; Base request handler protocol
(defgeneric handle-request (request)
  (:documentation "Handle an MCP request and return a result"))

;;; Base request class
(defclass mcp-request ()
  ((method :initarg :method :reader request-method :type string)
   (params :initarg :params :reader request-params :initform nil)
   (id :initarg :id :reader request-id :initform nil))
  (:documentation "Base class for all MCP requests"))

;;; Specific request types
(defclass initialize-request (mcp-request) ()
  (:documentation "Request to initialize the MCP connection"))

(defclass tools-list-request (mcp-request) ()
  (:documentation "Request to list available tools"))

(defclass tool-call-request (mcp-request)
  ((tool-name :initarg :tool-name :reader tool-name :type string)
   (arguments :initarg :arguments :reader tool-arguments))
  (:documentation "Request to call a specific tool"))

(defclass prompts-list-request (mcp-request) ()
  (:documentation "Request to list available prompts"))

(defclass prompts-get-request (mcp-request)
  ((prompt-name :initarg :prompt-name :reader prompt-name :type string)
   (prompt-args :initarg :prompt-args :reader prompt-args :initform nil))
  (:documentation "Request to get a specific prompt"))

(defclass resources-list-request (mcp-request) ()
  (:documentation "Request to list available resources"))

(defclass resources-read-request (mcp-request)
  ((uri :initarg :uri :reader resource-uri :type string))
  (:documentation "Request to read a specific resource"))

(defclass resources-templates-list-request (mcp-request) ()
  (:documentation "Request to list available resource templates"))

(defclass resources-subscribe-request (mcp-request)
  ((uri :initarg :uri :reader resource-uri :type string))
  (:documentation "Request to subscribe to a resource"))

(defclass resources-unsubscribe-request (mcp-request)
  ((uri :initarg :uri :reader resource-uri :type string))
  (:documentation "Request to unsubscribe from a resource"))

(defclass completion-complete-request (mcp-request)
  ((ref :initarg :ref :reader completion-ref)
   (argument :initarg :argument :reader completion-argument))
  (:documentation "Request for completion"))

(defclass logging-set-level-request (mcp-request)
  ((level :initarg :level :reader logging-level :type string))
  (:documentation "Request to set logging level"))

(defclass ping-request (mcp-request) ()
  (:documentation "Ping request"))

;;; Handler implementations

(defmethod handle-request ((req initialize-request))
  "Handle initialization request"
  (mcp-server.utilities:handle-initialize nil))

(defmethod handle-request ((req tools-list-request))
  "Handle tools list request"
  (mcp-server.tools:handle-tools-list nil))

(defmethod handle-request ((req tool-call-request))
  "Handle tool call request"
  (mcp-server.tools:handle-tool-call (tool-name req) (tool-arguments req)))

(defmethod handle-request ((req prompts-list-request))
  "Handle prompts list request"
  (mcp-server.prompts:handle-prompts-list nil))

(defmethod handle-request ((req prompts-get-request))
  "Handle prompt get request"
  (let ((params (alexandria:alist-hash-table
                 `(("name" . ,(prompt-name req))
                   ("arguments" . ,(prompt-args req)))
                 :test 'equal)))
    (mcp-server.prompts:handle-prompts-get params)))

(defmethod handle-request ((req resources-list-request))
  "Handle resources list request"
  (mcp-server.resources:handle-resources-list nil))

(defmethod handle-request ((req resources-read-request))
  "Handle resource read request"
  (mcp-server.resources:handle-resources-read (resource-uri req)))

(defmethod handle-request ((req resources-templates-list-request))
  "Handle resource templates list request"
  (make-instance 'mcp-server.types:list-resource-templates-result
                 :resource-templates (mcp-server.resources:get-available-resource-templates)))

(defmethod handle-request ((req resources-subscribe-request))
  "Handle resource subscribe request"
  (mcp-server.resources:handle-resources-subscribe (resource-uri req)))

(defmethod handle-request ((req resources-unsubscribe-request))
  "Handle resource unsubscribe request"
  (mcp-server.resources:handle-resources-unsubscribe (resource-uri req)))

(defmethod handle-request ((req completion-complete-request))
  "Handle completion request"
  (mcp-server.utilities:handle-completion (completion-ref req) (completion-argument req)))

(defmethod handle-request ((req logging-set-level-request))
  "Handle logging set level request"
  (let ((params (alexandria:alist-hash-table
                 `(("level" . ,(logging-level req)))
                 :test 'equal)))
    (mcp-server.utilities:handle-logging-set-level params)))

(defmethod handle-request ((req ping-request))
  "Handle ping request"
  (make-instance 'mcp-server.types:empty-result))

;;; Request factory function
(defun create-request-from-json (json-data)
  "Create appropriate request object from JSON data"
  (let ((method (gethash "method" json-data))
        (params (gethash "params" json-data))
        (id (gethash "id" json-data)))
    (cond
      ((string= method "initialize")
       (make-instance 'initialize-request
                      :method method
                      :params params
                      :id id))
      ((string= method "tools/list")
       (make-instance 'tools-list-request
                      :method method
                      :params params
                      :id id))
      ((string= method "tools/call")
       (make-instance 'tool-call-request
                      :method method
                      :params params
                      :id id
                      :tool-name (gethash "name" params)
                      :arguments (gethash "arguments" params)))
      ((string= method "prompts/list")
       (make-instance 'prompts-list-request
                      :method method
                      :params params
                      :id id))
      ((string= method "prompts/get")
       (make-instance 'prompts-get-request
                      :method method
                      :params params
                      :id id
                      :prompt-name (gethash "name" params)
                      :prompt-args (gethash "arguments" params)))
      ((string= method "resources/list")
       (make-instance 'resources-list-request
                      :method method
                      :params params
                      :id id))
      ((string= method "resources/read")
       (make-instance 'resources-read-request
                      :method method
                      :params params
                      :id id
                      :uri (gethash "uri" params)))
      ((string= method "resources/templates/list")
       (make-instance 'resources-templates-list-request
                      :method method
                      :params params
                      :id id))
      ((string= method "resources/subscribe")
       (make-instance 'resources-subscribe-request
                      :method method
                      :params params
                      :id id
                      :uri (gethash "uri" params)))
      ((string= method "resources/unsubscribe")
       (make-instance 'resources-unsubscribe-request
                      :method method
                      :params params
                      :id id
                      :uri (gethash "uri" params)))
      ((string= method "completion/complete")
       (make-instance 'completion-complete-request
                      :method method
                      :params params
                      :id id
                      :ref (gethash "ref" params)
                      :argument (gethash "argument" params)))
      ((string= method "logging/setLevel")
       (make-instance 'logging-set-level-request
                      :method method
                      :params params
                      :id id
                      :level (gethash "level" params)))
      ((string= method "ping")
       (make-instance 'ping-request
                      :method method
                      :params params
                      :id id))
      (t 
       (error 'method-not-found-error :method-name method)))))

;;; Process request with new protocol
(defun process-request-oop (json-data)
  "Process a request using the new OOP protocol"
  (handler-case
      (let* ((request (create-request-from-json json-data))
             (result (handle-request request)))
        (mcp-server.types:make-json-rpc-response (request-id request) result))
    (method-not-found-error (e)
      (mcp-server.types:make-json-rpc-error (gethash "id" json-data)
                          -32601
                          (format nil "Method not found: ~A" (method-name e))))
    (error (e)
      (mcp-server.types:make-json-rpc-error (gethash "id" json-data)
                          -32603
                          (format nil "Internal error: ~A" e)))))