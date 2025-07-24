;;;; Class Hierarchy for MCP Server Types
;;;; This implements OOP improvement #2: Class Hierarchy for Related Types

(in-package :mcp-server)

;;; Base classes for common patterns

(defclass mcp-list-result ()
  ((next-cursor :initarg :next-cursor :accessor next-cursor :initform nil))
  (:documentation "Base class for all list results with pagination support"))

(defclass mcp-content ()
  ((mime-type :initarg :mime-type :accessor content-mime-type :initform "text/plain")
   (text :initarg :text :accessor content-text :initform nil))
  (:documentation "Base class for all content types"))

(defclass mcp-named-entity ()
  ((name :initarg :name :accessor entity-name :type string)
   (description :initarg :description :accessor entity-description :type string))
  (:documentation "Base class for entities with name and description"))

(defclass mcp-identifiable ()
  ((id :initarg :id :accessor entity-id :type string))
  (:documentation "Base class for entities with unique identifiers"))


;;; Generic functions for polymorphic behavior

(defgeneric to-json-representation (object)
  (:documentation "Convert object to a representation suitable for JSON encoding"))

(defgeneric validate-entity (entity)
  (:documentation "Validate an entity's data"))

(defgeneric entity-type (entity)
  (:documentation "Return the type of the entity as a string"))

;;; Default implementations

(defmethod to-json-representation ((obj mcp-list-result))
  "Default JSON representation for list results"
  (let ((result (make-hash-table :test 'equal)))
    (when (next-cursor obj)
      (setf (gethash "nextCursor" result) (next-cursor obj)))
    result))

(defmethod to-json-representation ((obj mcp-content))
  "Default JSON representation for content"
  (let ((result (make-hash-table :test 'equal)))
    (when (content-mime-type obj)
      (setf (gethash "mimeType" result) (content-mime-type obj)))
    (when (content-text obj)
      (setf (gethash "text" result) (content-text obj)))
    result))

(defmethod to-json-representation ((obj mcp-named-entity))
  "Default JSON representation for named entities"
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "name" result) (entity-name obj))
    (setf (gethash "description" result) (entity-description obj))
    result))

(defmethod validate-entity ((entity mcp-named-entity))
  "Validate that named entities have required fields"
  (unless (and (entity-name entity)
               (> (length (entity-name entity)) 0))
    (error 'invalid-params-error 
           :message "Named entity must have a non-empty name"))
  t)

(defmethod validate-entity ((entity mcp-identifiable))
  "Validate that identifiable entities have an ID"
  (unless (and (entity-id entity)
               (> (length (entity-id entity)) 0))
    (error 'invalid-params-error 
           :message "Identifiable entity must have a non-empty ID"))
  t)

;;; Specialized implementations for each type


;;; Migration helpers

(defun migrate-to-hierarchical-types ()
  "Helper function to migrate existing code to use new hierarchical types"
  ;; This function would contain logic to help transition from old types to new
  ;; For now, it's a placeholder
  (format t "Ready to use hierarchical types. Update code to use -v2 classes.~%"))

;;; Mixins for additional functionality

(defclass timestamped-mixin ()
  ((created-at :initarg :created-at 
               :accessor created-at 
               :initform (local-time:now))
   (updated-at :initarg :updated-at 
               :accessor updated-at 
               :initform (local-time:now)))
  (:documentation "Mixin to add timestamp tracking to any class"))

(defclass cacheable-mixin ()
  ((cache-key :initarg :cache-key :accessor cache-key)
   (cache-ttl :initarg :cache-ttl :accessor cache-ttl :initform 300))
  (:documentation "Mixin to make objects cacheable"))

(defmethod (setf updated-at) :before (new-value (obj timestamped-mixin))
  "Automatically update the updated-at timestamp"
  (declare (ignore new-value))
  (setf (slot-value obj 'updated-at) (local-time:now)))

