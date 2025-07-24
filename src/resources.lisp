;; ====================================================================
;; Resources Implementation (src/resources.lisp)
;; ====================================================================

(in-package #:mcp-server.resources)

(defvar *resources-cache* nil
  "Cache for loaded resources")

(defun load-resources ()
  "Load resources from template data"
  (unless *resources-cache*
    (setf *resources-cache* (load-resources-template)))
  *resources-cache*)

(defun convert-resource-from-json (resource-json)
  "Convert JSON resource definition to resource object"
  (make-instance 'resource
                 :uri (gethash "uri" resource-json)
                 :name (gethash "name" resource-json)
                 :description (gethash "description" resource-json)
                 :mime-type (gethash "mimeType" resource-json)))

(defun get-available-resources ()
  "Get list of available resources"
  (map 'list #'convert-resource-from-json (load-resources)))

(defun handle-resources-list (request)
  "Handle resources/list request"
  (declare (ignore request))
  (make-instance 'list-resources-result
                 :resources (get-available-resources)))

(defun handle-resource-read (request)
  "Handle resources/read request"
  (let ((uri (gethash "uri" request)))
    (make-instance 'read-resource-result
                   :contents (make-instance 'resource-content
                                           :uri uri
                                           :mime-type "text/plain"
                                           :text "2024-11-28T08:19:18.974368Z,INFO,main,this is message"))))

(defun handle-resources-read (uri)
  "Handle resources/read request with uri parameter"
  (make-instance 'read-resource-result
                 :contents (make-instance 'resource-content
                                         :uri uri
                                         :mime-type "text/plain"
                                         :text "2024-11-28T08:19:18.974368Z,INFO,main,this is message")))

(defun get-available-resource-templates ()
  "Get list of available resource templates"
  ;; For now, return empty list - can be expanded later
  '())

(defun handle-resources-subscribe (uri)
  "Handle resources/subscribe request"
  (declare (ignore uri))
  ;; For now, just return success
  (make-instance 'empty-result))

(defun handle-resources-unsubscribe (uri)
  "Handle resources/unsubscribe request"
  (declare (ignore uri))
  ;; For now, just return success
  (make-instance 'empty-result))

