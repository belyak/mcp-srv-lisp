;; ====================================================================
;; Utilities (src/utilities.lisp)
;; ====================================================================

(defpackage #:mcp-server.utilities
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:initialize-request
                #:initialize-result
                #:server-capabilities
                #:implementation
                #:empty-result
                #:connect-result
                #:disconnect-result
                #:execute-line-result
                #:cancelled-notification)
  (:import-from #:mcp-server.constants
                #:+protocol-version+
                #:+server-name+
                #:+server-version+)
  (:export #:handle-initialize
           #:handle-ping
           #:handle-connect
           #:handle-disconnect
           #:handle-execute-line
           #:handle-logging-set-level
           #:handle-roots-list
           #:handle-cancelled-notification
           #:graceful-shutdown
           #:notify))

(in-package #:mcp-server.utilities)

(defun handle-initialize (request)
  "Handle initialize request from client"
  (declare (ignore request))
  (make-instance 'initialize-result
                 :protocol-version +protocol-version+
                 :server-info (make-instance 'implementation
                                             :name +server-name+
                                             :version +server-version+)
                 :capabilities (make-instance 'server-capabilities
                                              :prompts t
                                              :resources t
                                              :tools t)))

(defun handle-ping (request)
  "Handle ping request"
  (declare (ignore request))
  (make-instance 'empty-result))

(defun handle-connect (request)
  "Handle connect request"
  (declare (ignore request))
  (make-instance 'connect-result
                 :connection-id "fake_connection_id"))

(defun handle-disconnect (request)
  "Handle disconnect request"
  (declare (ignore request))
  (make-instance 'disconnect-result
                 :success t))

(defun handle-execute-line (request)
  "Handle execute line request"
  (let ((line (gethash "line" request "")))
    (make-instance 'execute-line-result
                   :output (format nil "fake_output: ~A" line)
                   :is-error nil)))

(defun handle-logging-set-level (request)
  "Handle logging set level request"
  (declare (ignore request))
  (make-instance 'empty-result))

(defun handle-roots-list (request)
  "Handle roots list request"
  (declare (ignore request))
  `(("roots" . #((("name" . "my project")
                  ("url" . "file:///home/user/projects/my-project"))))))

(defun handle-cancelled-notification (params)
  "Handle cancelled notification from client"
  (declare (ignore params))
  ;; Log cancellation
  nil)

(defun graceful-shutdown ()
  "Handle graceful shutdown"
  (format *error-output* "[MCP-LISP] Graceful shutdown initiated~%")
  ;; Cleanup logic here
  nil)

(defun notify (method &optional params)
  "Send notification to client"
  (let ((notification (yason:with-output-to-string* ()
                        (yason:with-object
                          (yason:encode-object-element "jsonrpc" "2.0")
                          (yason:encode-object-element "method" method)
                          (when params
                            (yason:encode-object-element "params" params))))))
    (format t "~A~%" notification)
    (force-output)))

