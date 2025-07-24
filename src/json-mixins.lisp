;;;; JSON Serialization Mixins for MCP Server
;;;; This implements OOP improvement #3: Mixin Classes for JSON Serialization

(in-package :mcp-server)

;;; JSON serialization mixin base class
(defclass json-serializable ()
  ()
  (:documentation "Base mixin for objects that can be serialized to JSON"))

(defgeneric to-json-alist (object)
  (:documentation "Convert object to association list for JSON encoding"))

(defgeneric from-json-alist (class alist)
  (:documentation "Create object from JSON association list"))

;;; Default implementation using existing encode-to-json
(defmethod encode-to-json ((obj json-serializable))
  "Default JSON encoding using to-json-alist"
  (alexandria:alist-hash-table (to-json-alist obj) :test 'equal))

;;; Auto-serialization mixin for simple objects
(defclass auto-json-serializable (json-serializable)
  ()
  (:documentation "Mixin that automatically serializes all slots to JSON"))

(defun kebab-to-camel-case (string)
  "Convert kebab-case to camelCase"
  (let ((parts (split-sequence:split-sequence #\- string)))
    (if (null parts)
        string
        (apply #'concatenate 'string
               (first parts)
               (mapcar #'string-capitalize (rest parts))))))

(defun camel-to-kebab-case (string)
  "Convert camelCase to kebab-case"
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          do (if (and (upper-case-p char) (> i 0))
                 (progn
                   (write-char #\- out)
                   (write-char (char-downcase char) out))
                 (write-char (char-downcase char) out)))))

(defmethod to-json-alist ((obj auto-json-serializable))
  "Automatically convert all bound slots to JSON alist"
  (loop for slot in (closer-mop:class-slots (class-of obj))
        for slot-name = (closer-mop:slot-definition-name slot)
        for json-name = (kebab-to-camel-case (string-downcase (string slot-name)))
        when (slot-boundp obj slot-name)
        collect (cons json-name 
                     (let ((value (slot-value obj slot-name)))
                       (cond
                         ;; Handle nested objects
                         ((typep value 'json-serializable)
                          (encode-to-json value))
                         ;; Handle lists of objects
                         ((and (listp value)
                               (every (lambda (v) (typep v 'json-serializable)) value))
                          (map 'vector #'encode-to-json value))
                         ;; Convert lists to vectors for JSON arrays
                         ((listp value) (coerce value 'vector))
                         ;; Everything else as-is
                         (t value))))))

;;; Selective serialization mixin
(defclass selective-json-serializable (json-serializable)
  ()
  (:documentation "Mixin for selective slot serialization with custom naming"))

(defgeneric json-slots (object)
  (:documentation "Return list of (slot-name . json-name) pairs to serialize")
  (:method ((obj selective-json-serializable))
    ;; Default: return empty list, subclasses should override
    '()))

(defmethod to-json-alist ((obj selective-json-serializable))
  "Serialize only specified slots with custom JSON names"
  (loop for (slot-name . json-name) in (json-slots obj)
        when (slot-boundp obj slot-name)
        collect (cons json-name 
                     (let ((value (slot-value obj slot-name)))
                       (cond
                         ((typep value 'json-serializable)
                          (encode-to-json value))
                         ((and (listp value)
                               (every (lambda (v) (typep v 'json-serializable)) value))
                          (map 'vector #'encode-to-json value))
                         ((listp value) (coerce value 'vector))
                         (t value))))))

;;; Cached JSON serialization mixin
(defclass cached-json-serializable (json-serializable)
  ((json-cache :initform nil :accessor json-cache)
   (cache-valid-p :initform nil :accessor cache-valid-p))
  (:documentation "Mixin that caches JSON representation"))

(defmethod encode-to-json ((obj cached-json-serializable))
  "Return cached JSON or compute and cache it"
  (unless (cache-valid-p obj)
    (setf (json-cache obj) (call-next-method)
          (cache-valid-p obj) t))
  (json-cache obj))

(defmethod (setf slot-value-using-class) :after (new-value class (obj cached-json-serializable) slot)
  "Invalidate cache when any slot changes"
  (declare (ignore new-value class slot))
  (setf (cache-valid-p obj) nil))

;;; Example implementations using the mixins

;; Tool class using auto-serialization
(defclass tool-v3 (mcp-server.types:tool auto-json-serializable)
  ()
  (:documentation "Tool with automatic JSON serialization"))

;; Prompt class using selective serialization
(defclass prompt-v3 (mcp-server.types:prompt selective-json-serializable)
  ()
  (:documentation "Prompt with selective JSON serialization"))

(defmethod json-slots ((prompt prompt-v3))
  "Specify which slots to serialize for prompts"
  '((name . "name")
    (description . "description")
    (arguments . "arguments")))

;; Resource class using cached serialization
(defclass resource-v3 (mcp-server.types:resource cached-json-serializable)
  ()
  (:documentation "Resource with cached JSON serialization"))

(defmethod to-json-alist ((resource resource-v3))
  "Custom JSON serialization for resources"
  `(("uri" . ,(resource-uri resource))
    ("name" . ,(resource-name resource))
    ("description" . ,(resource-description resource))
    ,@(when (resource-mime-type resource)
        `(("mimeType" . ,(resource-mime-type resource))))))

;;; Utility functions for working with JSON mixins

(defun make-json-object (class &rest initargs)
  "Create an object that can be serialized to JSON"
  (apply #'make-instance class initargs))

(defmacro define-json-class (name superclasses slots &rest options)
  "Define a class that automatically includes JSON serialization"
  (let ((json-superclasses (if (find 'auto-json-serializable superclasses)
                              superclasses
                              (append superclasses '(auto-json-serializable)))))
    `(defclass ,name ,json-superclasses
       ,slots
       ,@options)))

;;; Integration helpers

(defun upgrade-to-json-serializable (object target-class)
  "Upgrade an existing object to use JSON serialization mixins"
  (change-class object target-class))

(defun json-encode-object (object)
  "Encode any object to JSON, upgrading if necessary"
  (cond
    ((typep object 'json-serializable)
     (encode-to-json object))
    ((typep object 'mcp-server.types:tool)
     (encode-to-json (upgrade-to-json-serializable object 'tool-v3)))
    ((typep object 'mcp-server.types:prompt)
     (encode-to-json (upgrade-to-json-serializable object 'prompt-v3)))
    ((typep object 'mcp-server.types:resource)
     (encode-to-json (upgrade-to-json-serializable object 'resource-v3)))
    (t
     (error "Cannot JSON encode object of type ~A" (type-of object)))))