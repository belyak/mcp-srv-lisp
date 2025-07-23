;; ====================================================================
;; Resources Implementation (src/resources.lisp)
;; ====================================================================

(defpackage #:mcp-server.resources
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:resource
                #:resource-content
                #:list-resources-result
                #:read-resource-result)
  (:import-from #:mcp-server.templates
                #:load-resources-template)
  (:export #:handle-resources-list
           #:handle-resource-read
           #:get-available-resources))

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
                   :content (make-instance 'resource-content
                                           :uri uri
                                           :mime-type "text/plain"
                                           :text "2024-11-28T08:19:18.974368Z,INFO,main,this is message"))))

