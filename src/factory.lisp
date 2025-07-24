;;;; Factory Pattern for Object Creation
;;;; This implements OOP improvement #6: Factory Pattern for Object Creation

(in-package :mcp-server)

;;; Generic factory protocol
(defgeneric create-from-json (type json-data)
  (:documentation "Factory method for creating objects from JSON data"))

(defgeneric create-from-params (type params)
  (:documentation "Factory method for creating objects from parameter hash"))

;;; Abstract factory class
(defclass mcp-factory ()
  ((type-mapping :initform (make-hash-table :test 'equal) :accessor factory-type-mapping))
  (:documentation "Base factory class for creating MCP objects"))

(defgeneric register-type (factory type-key class)
  (:documentation "Register a type mapping in the factory"))

(defgeneric create-object (factory type-key data)
  (:documentation "Create an object using the factory"))

(defmethod register-type ((factory mcp-factory) type-key class)
  "Register a type mapping"
  (setf (gethash type-key (factory-type-mapping factory)) class))

(defmethod create-object ((factory mcp-factory) type-key data)
  "Create an object using registered type mapping"
  (let ((class (gethash type-key (factory-type-mapping factory))))
    (if class
        (create-from-json class data)
        (error 'invalid-params-error 
               :message (format nil "Unknown type: ~A" type-key)))))

;;; Request factory
(defclass request-factory (mcp-factory)
  ()
  (:documentation "Factory for creating MCP request objects"))

(defvar *request-factory* (make-instance 'request-factory)
  "Global request factory instance")


;;; Tool factory
(defclass tool-factory (mcp-factory)
  ()
  (:documentation "Factory for creating tool objects"))

(defvar *tool-factory* (make-instance 'tool-factory)
  "Global tool factory instance")

;; Register known tool types
(register-type *tool-factory* "get_current_time_in_city" 'time-tool)
(register-type *tool-factory* "calculator" 'calculator-tool)

(defmethod create-from-json ((type (eql 'tool)) json-data)
  "Create tool object from JSON data"
  (let* ((tool-name (gethash "name" json-data))
         (tool-class (gethash tool-name (factory-type-mapping *tool-factory*))))
    (if tool-class
        ;; Create specific tool type
        (make-instance tool-class)
        ;; Create generic tool
        (make-instance 'mcp-tool
                       :name tool-name
                       :description (gethash "description" json-data)
                       :input-schema (create-from-json 'tool-input-schema 
                                                     (gethash "inputSchema" json-data))))))

(defmethod create-from-json ((type (eql 'tool-input-schema)) json-data)
  "Create tool input schema from JSON data"
  (when json-data
    (make-instance 'tool-input-schema
                   :type (gethash "type" json-data "object")
                   :properties (mapcar (lambda (prop-data)
                                       (create-from-json 'tool-input-schema-property prop-data))
                                     (coerce (gethash "properties" json-data '()) 'list))
                   :required (coerce (gethash "required" json-data '()) 'list))))

(defmethod create-from-json ((type (eql 'tool-input-schema-property)) json-data)
  "Create tool input schema property from JSON data"
  (make-instance 'tool-input-schema-property
                 :type (gethash "type" json-data)
                 :description (gethash "description" json-data)
                 :enum-values (when (gethash "enum" json-data)
                               (coerce (gethash "enum" json-data) 'list))))

;;; Prompt factory
(defmethod create-from-json ((type (eql 'prompt)) json-data)
  "Create prompt object from JSON data"
  (make-instance 'prompt
                 :name (gethash "name" json-data)
                 :description (gethash "description" json-data)
                 :arguments (mapcar (lambda (arg-data)
                                    (create-from-json 'prompt-argument arg-data))
                                  (coerce (gethash "arguments" json-data '()) 'list))))

(defmethod create-from-json ((type (eql 'prompt-argument)) json-data)
  "Create prompt argument from JSON data" 
  (make-instance 'prompt-argument
                 :name (gethash "name" json-data)
                 :description (gethash "description" json-data)
                 :required (gethash "required" json-data)))

;;; Resource factory
(defmethod create-from-json ((type (eql 'resource)) json-data)
  "Create resource object from JSON data"
  (make-instance 'resource
                 :uri (gethash "uri" json-data)
                 :name (gethash "name" json-data)
                 :description (gethash "description" json-data)
                 :mime-type (gethash "mimeType" json-data)))

;;; Content factories
(defmethod create-from-json ((type (eql 'call-tool-result-content)) json-data)
  "Create tool result content from JSON data"
  (make-instance 'call-tool-result-content
                 :type (gethash "type" json-data "text")
                 :text (gethash "text" json-data)
                 :data (gethash "data" json-data)))

(defmethod create-from-json ((type (eql 'resource-content)) json-data)
  "Create resource content from JSON data"
  (make-instance 'resource-content
                 :uri (gethash "uri" json-data)
                 :mime-type (gethash "mimeType" json-data)
                 :text (gethash "text" json-data)
                 :blob (gethash "blob" json-data)))

(defmethod create-from-json ((type (eql 'prompt-message-content)) json-data)
  "Create prompt message content from JSON data"
  (make-instance 'prompt-message-content
                 :type (gethash "type" json-data "text")
                 :text (gethash "text" json-data)))

;;; Result factories
(defmethod create-from-json ((type (eql 'call-tool-result)) json-data)
  "Create tool result from JSON data"
  (make-instance 'call-tool-result
                 :content (mapcar (lambda (content-data)
                                  (create-from-json 'call-tool-result-content content-data))
                                (coerce (gethash "content" json-data '()) 'list))
                 :is-error (gethash "isError" json-data)))

;;; Convenience functions and macros

(defun create-tool-from-template (template)
  "Create a tool instance from a template hash table"
  (create-from-json 'tool template))

(defun create-prompt-from-template (template)
  "Create a prompt instance from a template hash table"
  (create-from-json 'prompt template))

(defun create-resource-from-template (template)
  "Create a resource instance from a template hash table"
  (create-from-json 'resource template))

(defmacro define-factory-type (factory type-key class-name)
  "Macro to register a type in a factory"
  `(register-type ,factory ,type-key ',class-name))

;;; Builder pattern for complex objects

(defclass mcp-builder ()
  ((object :initform nil :accessor builder-object))
  (:documentation "Base builder class for complex MCP objects"))

(defgeneric build (builder)
  (:documentation "Build and return the final object"))

(defgeneric reset (builder)
  (:documentation "Reset the builder for reuse"))

(defclass tool-builder (mcp-builder)
  ((name :initform nil :accessor tool-name)
   (description :initform nil :accessor tool-description)
   (input-schema :initform nil :accessor tool-input-schema))
  (:documentation "Builder for creating tool objects"))

(defmethod build ((builder tool-builder))
  "Build a tool from the builder state"
  (make-instance 'mcp-tool
                 :name (tool-name builder)
                 :description (tool-description builder)
                 :input-schema (tool-input-schema builder)))

(defmethod reset ((builder tool-builder))
  "Reset the tool builder"
  (setf (tool-name builder) nil
        (tool-description builder) nil
        (tool-input-schema builder) nil))

(defun make-tool-builder ()
  "Create a new tool builder"
  (make-instance 'tool-builder))

;;; Fluent interface for builders

(defgeneric with-name (builder name)
  (:documentation "Set the name for the object being built"))

(defgeneric with-description (builder description)
  (:documentation "Set the description for the object being built"))

(defgeneric with-schema (builder schema)
  (:documentation "Set the schema for the object being built"))

(defmethod with-name ((builder tool-builder) name)
  "Set tool name and return builder for chaining"
  (setf (tool-name builder) name)
  builder)

(defmethod with-description ((builder tool-builder) description)
  "Set tool description and return builder for chaining"
  (setf (tool-description builder) description)
  builder)

(defmethod with-schema ((builder tool-builder) schema)
  "Set tool schema and return builder for chaining"
  (setf (tool-input-schema builder) schema)
  builder)