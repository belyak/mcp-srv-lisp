;; ====================================================================
;; Types and Data Structures (src/types.lisp)
;; ====================================================================

(defpackage #:mcp-server.types
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.constants
                #:+jsonrpc-version+)
  (:export #:json-rpc-request
           #:json-rpc-response
           #:json-rpc-error
           #:json-rpc-notification
           #:initialize-request
           #:initialize-result
           #:client-capabilities
           #:server-capabilities
           #:implementation
           #:tool
           #:tool-input-schema
           #:tool-input-schema-property
           #:call-tool-result
           #:call-tool-result-content
           #:list-tools-result
           #:prompt
           #:prompt-argument
           #:prompt-result
           #:prompt-message
           #:prompt-message-content
           #:list-prompts-result
           #:resource
           #:resource-content
           #:list-resources-result
           #:read-resource-result
           #:connect-result
           #:disconnect-result
           #:execute-line-result
           #:empty-result
           #:cancelled-notification
           #:make-json-rpc-response
           #:make-json-rpc-error
           #:encode-to-json
           #:parse-from-json))

(in-package #:mcp-server.types)

;; JSON-RPC Base Types
(defclass json-rpc-request ()
  ((id :initarg :id :accessor request-id)
   (method :initarg :method :accessor request-method)
   (params :initarg :params :accessor request-params :initform nil)))

(defclass json-rpc-response ()
  ((id :initarg :id :accessor response-id)
   (result :initarg :result :accessor response-result)))

(defclass json-rpc-error ()
  ((id :initarg :id :accessor error-id)
   (code :initarg :code :accessor error-code)
   (message :initarg :message :accessor error-message)
   (data :initarg :data :accessor error-data :initform nil)))

(defclass json-rpc-notification ()
  ((method :initarg :method :accessor notification-method)
   (params :initarg :params :accessor notification-params :initform nil)))

;; MCP Protocol Types
(defclass implementation ()
  ((name :initarg :name :accessor impl-name)
   (version :initarg :version :accessor impl-version)))

(defclass client-capabilities ()
  ((experimental :initarg :experimental :accessor client-experimental :initform nil)
   (roots :initarg :roots :accessor client-roots :initform nil)
   (sampling :initarg :sampling :accessor client-sampling :initform nil)))

(defclass server-capabilities ()
  ((experimental :initarg :experimental :accessor server-experimental :initform nil)
   (prompts :initarg :prompts :accessor server-prompts :initform nil)
   (resources :initarg :resources :accessor server-resources :initform nil)
   (tools :initarg :tools :accessor server-tools :initform nil)
   (roots :initarg :roots :accessor server-roots :initform nil)
   (sampling :initarg :sampling :accessor server-sampling :initform nil)
   (logging :initarg :logging :accessor server-logging :initform nil)))

(defclass initialize-request ()
  ((protocol-version :initarg :protocol-version :accessor init-protocol-version)
   (capabilities :initarg :capabilities :accessor init-capabilities)
   (client-info :initarg :client-info :accessor init-client-info)))

(defclass initialize-result ()
  ((protocol-version :initarg :protocol-version :accessor result-protocol-version)
   (capabilities :initarg :capabilities :accessor result-capabilities)
   (server-info :initarg :server-info :accessor result-server-info)
   (instructions :initarg :instructions :accessor result-instructions :initform nil)))

;; Tool Types
(defclass tool-input-schema-property ()
  ((type-name :initarg :type-name :accessor prop-type-name :initform nil)
   (enum-values :initarg :enum-values :accessor prop-enum-values :initform nil)
   (description :initarg :description :accessor prop-description :initform nil)))

(defclass tool-input-schema ()
  ((type-name :initarg :type-name :accessor schema-type-name)
   (properties :initarg :properties :accessor schema-properties)
   (required :initarg :required :accessor schema-required)))

(defclass tool ()
  ((name :initarg :name :accessor tool-name)
   (description :initarg :description :accessor tool-description :initform nil)
   (input-schema :initarg :input-schema :accessor tool-input-schema)))

(defclass call-tool-result-content ()
  ((type-name :initarg :type :accessor content-type)
   (text :initarg :text :accessor content-text :initform nil)
   (data :initarg :data :accessor content-data :initform nil)
   (mime-type :initarg :mime-type :accessor content-mime-type :initform nil)))

(defclass call-tool-result ()
  ((content :initarg :content :accessor result-content)
   (is-error :initarg :is-error :accessor result-is-error :initform nil)))

(defclass list-tools-result ()
  ((tools :initarg :tools :accessor tools-list)
   (next-cursor :initarg :next-cursor :accessor tools-next-cursor :initform nil)))

;; Prompt Types
(defclass prompt-argument ()
  ((name :initarg :name :accessor arg-name)
   (description :initarg :description :accessor arg-description :initform nil)
   (required :initarg :required :accessor arg-required :initform nil)))

(defclass prompt ()
  ((name :initarg :name :accessor prompt-name)
   (description :initarg :description :accessor prompt-description :initform nil)
   (arguments :initarg :arguments :accessor prompt-arguments :initform nil)))

(defclass prompt-message-content ()
  ((type-name :initarg :type :accessor msg-content-type)
   (text :initarg :text :accessor msg-content-text)))

(defclass prompt-message ()
  ((role :initarg :role :accessor msg-role)
   (content :initarg :content :accessor msg-content)))

(defclass prompt-result ()
  ((description :initarg :description :accessor prompt-result-description)
   (messages :initarg :messages :accessor prompt-result-messages :initform nil)))

(defclass list-prompts-result ()
  ((prompts :initarg :prompts :accessor prompts-list)
   (next-cursor :initarg :next-cursor :accessor prompts-next-cursor :initform nil)))

;; Resource Types
(defclass resource ()
  ((uri :initarg :uri :accessor resource-uri)
   (name :initarg :name :accessor resource-name)
   (description :initarg :description :accessor resource-description :initform nil)
   (mime-type :initarg :mime-type :accessor resource-mime-type :initform nil)))

(defclass resource-content ()
  ((uri :initarg :uri :accessor content-uri)
   (mime-type :initarg :mime-type :accessor content-mime-type :initform nil)
   (text :initarg :text :accessor content-text :initform nil)
   (blob :initarg :blob :accessor content-blob :initform nil)))

(defclass list-resources-result ()
  ((resources :initarg :resources :accessor resources-list)
   (next-cursor :initarg :next-cursor :accessor resources-next-cursor :initform nil)))

(defclass read-resource-result ()
  ((content :initarg :content :accessor resource-content)))

;; Session Types
(defclass connect-result ()
  ((connection-id :initarg :connection-id :accessor connection-id)))

(defclass disconnect-result ()
  ((success :initarg :success :accessor disconnect-success)))

(defclass execute-line-result ()
  ((output :initarg :output :accessor exec-output)
   (is-error :initarg :is-error :accessor exec-is-error)))

;; Misc Types
(defclass empty-result () ())

(defclass cancelled-notification ()
  ((request-id :initarg :request-id :accessor cancel-request-id)
   (reason :initarg :reason :accessor cancel-reason :initform nil)))

;; Utility Functions
(defun make-json-rpc-response (id result)
  (make-instance 'json-rpc-response :id id :result result))

(defun make-json-rpc-error (id code message &optional data)
  (make-instance 'json-rpc-error :id id :code code :message message :data data))

;; JSON Encoding/Decoding Functions
(defgeneric encode-to-json (object)
  (:documentation "Encode an object to JSON"))

(defmethod encode-to-json (object)
  "Default method for simple objects"
  object)

(defmethod encode-to-json ((response json-rpc-response))
  (yason:with-output-to-string* ()
    (yason:with-object
      (yason:encode-object-element "jsonrpc" +jsonrpc-version+)
      (yason:encode-object-element "id" (response-id response))
      (yason:encode-object-element "result" (encode-to-json (response-result response))))))

(defmethod encode-to-json ((error json-rpc-error))
  (yason:with-output-to-string* ()
    (yason:with-object
      (yason:encode-object-element "jsonrpc" +jsonrpc-version+)
      (yason:encode-object-element "id" (error-id error))
      (yason:encode-object-element "error"
        (yason:with-output-to-string* ()
          (yason:with-object
            (yason:encode-object-element "code" (error-code error))
            (yason:encode-object-element "message" (error-message error))
            (when (error-data error)
              (yason:encode-object-element "data" (error-data error)))))))))

(defmethod encode-to-json ((notification json-rpc-notification))
  (yason:with-output-to-string* ()
    (yason:with-object
      (yason:encode-object-element "jsonrpc" +jsonrpc-version+)
      (yason:encode-object-element "method" (notification-method notification))
      (when (notification-params notification)
        (yason:encode-object-element "params" (encode-to-json (notification-params notification)))))))

;; Add more encoding methods for other types as needed...

(defun parse-from-json (json-string)
  "Parse JSON string into appropriate objects"
  (let ((parsed (yason:parse json-string)))
    (cond
      ((and (hash-table-p parsed)
            (gethash "method" parsed)
            (not (gethash "id" parsed)))
       ;; Notification
       (make-instance 'json-rpc-notification
                      :method (gethash "method" parsed)
                      :params (gethash "params" parsed)))
      ((and (hash-table-p parsed)
            (gethash "method" parsed)
            (gethash "id" parsed))
       ;; Request
       (make-instance 'json-rpc-request
                      :id (gethash "id" parsed)
                      :method (gethash "method" parsed)
                      :params (gethash "params" parsed)))
      (t parsed))))
