;;;; Condition System for MCP Server
;;;; This implements OOP improvement #7: Condition System for Error Handling

(in-package :mcp-server)

;;; Base MCP error condition
(define-condition mcp-error (error)
  ((code :initarg :code :reader error-code :type integer)
   (message :initarg :message :reader error-message :type string)
   (data :initarg :data :reader error-data :initform nil))
  (:documentation "Base condition for all MCP-related errors"))

;;; Specific error types

(define-condition method-not-found-error (mcp-error)
  ((method-name :initarg :method-name :reader method-name :type string))
  (:default-initargs :code -32601)
  (:report (lambda (condition stream)
             (format stream "Method not found: ~A" 
                     (method-name condition))))
  (:documentation "Raised when an unknown method is requested"))

(define-condition invalid-params-error (mcp-error)
  ()
  (:default-initargs :code -32602)
  (:documentation "Raised when request parameters are invalid"))

(define-condition missing-required-parameter (invalid-params-error)
  ((parameter :initarg :parameter :reader missing-parameter :type string))
  (:report (lambda (condition stream)
             (format stream "Missing required parameter: ~A" 
                     (missing-parameter condition))))
  (:documentation "Raised when a required parameter is missing"))

(define-condition invalid-parameter-type (invalid-params-error)
  ((parameter :initarg :parameter :reader parameter-name :type string)
   (expected-type :initarg :expected-type :reader expected-type)
   (actual-value :initarg :actual-value :reader actual-value))
  (:report (lambda (condition stream)
             (format stream "Invalid type for parameter ~A: expected ~A, got ~A"
                     (parameter-name condition)
                     (expected-type condition)
                     (type-of (actual-value condition)))))
  (:documentation "Raised when a parameter has the wrong type"))

(define-condition internal-error (mcp-error)
  ()
  (:default-initargs :code -32603)
  (:documentation "Raised for internal server errors"))

(define-condition json-parse-error (mcp-error)
  ()
  (:default-initargs :code -32700)
  (:documentation "Raised when JSON parsing fails"))

(define-condition invalid-request (mcp-error)
  ()
  (:default-initargs :code -32600)
  (:documentation "Raised when the JSON-RPC request is invalid"))

(define-condition tool-not-found-error (mcp-error)
  ((tool-name :initarg :tool-name :reader tool-name :type string))
  (:default-initargs :code -32602)
  (:report (lambda (condition stream)
             (format stream "Tool not found: ~A" 
                     (tool-name condition))))
  (:documentation "Raised when a requested tool doesn't exist"))

(define-condition prompt-not-found-error (mcp-error)
  ((prompt-name :initarg :prompt-name :reader prompt-name :type string))
  (:default-initargs :code -32602)
  (:report (lambda (condition stream)
             (format stream "Prompt not found: ~A" 
                     (prompt-name condition))))
  (:documentation "Raised when a requested prompt doesn't exist"))

(define-condition resource-not-found-error (mcp-error)
  ((uri :initarg :uri :reader resource-uri :type string))
  (:default-initargs :code -32602)
  (:report (lambda (condition stream)
             (format stream "Resource not found: ~A" 
                     (resource-uri condition))))
  (:documentation "Raised when a requested resource doesn't exist"))

(define-condition resource-error (mcp-error)
  ((uri :initarg :uri :reader resource-uri :type string)
   (reason :initarg :reason :reader error-reason :type string))
  (:default-initargs :code -32603)
  (:report (lambda (condition stream)
             (format stream "Resource error for ~A: ~A"
                     (resource-uri condition)
                     (error-reason condition))))
  (:documentation "Raised when there's an error accessing a resource"))

;;; Helper functions for error handling

(defun make-error-response (id code message &optional data)
  "Create a JSON-RPC error response"
  (make-json-rpc-error id code message data))

(defun handle-mcp-condition (condition request-id)
  "Convert an MCP condition to a JSON-RPC error response"
  (make-error-response request-id
                      (error-code condition)
                      (error-message condition)
                      (error-data condition)))

;;; Error handling macro

(defmacro with-mcp-error-handling ((request-id) &body body)
  "Execute body with MCP error handling"
  `(handler-case
       (progn ,@body)
     (mcp-error (e)
       (handle-mcp-condition e ,request-id))
     (error (e)
       (make-error-response ,request-id
                           -32603
                           (format nil "Internal error: ~A" e)))))