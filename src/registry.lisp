;;;; Registry Pattern for MCP Server Extensions
;;;; This implements OOP improvement #4: Registry Pattern for Extensions

(in-package :mcp-server)

;;; Extensible registry for MCP components
(defclass mcp-registry ()
  ((tools :initform (make-hash-table :test 'equal) :accessor registry-tools)
   (prompts :initform (make-hash-table :test 'equal) :accessor registry-prompts)
   (resources :initform (make-hash-table :test 'equal) :accessor registry-resources)
   (resource-templates :initform (make-hash-table :test 'equal) :accessor registry-resource-templates)
   (plugins :initform (make-hash-table :test 'equal) :accessor registry-plugins))
  (:documentation "Central registry for all MCP components"))

;;; Registry protocol
(defgeneric register-component (registry component-type name component)
  (:documentation "Register a component in the registry"))

(defgeneric unregister-component (registry component-type name)
  (:documentation "Remove a component from the registry"))

(defgeneric find-component (registry component-type name)
  (:documentation "Find a component by name"))

(defgeneric list-components (registry component-type)
  (:documentation "List all components of a given type"))

(defgeneric component-exists-p (registry component-type name)
  (:documentation "Check if a component exists"))

;;; Implementation for different component types

(defmethod register-component ((registry mcp-registry) (type (eql :tool)) name component)
  "Register a tool in the registry"
  (setf (gethash name (registry-tools registry)) component))

(defmethod register-component ((registry mcp-registry) (type (eql :prompt)) name component)
  "Register a prompt in the registry"
  (setf (gethash name (registry-prompts registry)) component))

(defmethod register-component ((registry mcp-registry) (type (eql :resource)) name component)
  "Register a resource in the registry"
  (setf (gethash name (registry-resources registry)) component))

(defmethod register-component ((registry mcp-registry) (type (eql :resource-template)) name component)
  "Register a resource template in the registry"
  (setf (gethash name (registry-resource-templates registry)) component))

(defmethod register-component ((registry mcp-registry) (type (eql :plugin)) name component)
  "Register a plugin in the registry"
  (setf (gethash name (registry-plugins registry)) component))

(defmethod unregister-component ((registry mcp-registry) (type (eql :tool)) name)
  "Remove a tool from the registry"
  (remhash name (registry-tools registry)))

(defmethod unregister-component ((registry mcp-registry) (type (eql :prompt)) name)
  "Remove a prompt from the registry"
  (remhash name (registry-prompts registry)))

(defmethod unregister-component ((registry mcp-registry) (type (eql :resource)) name)
  "Remove a resource from the registry"
  (remhash name (registry-resources registry)))

(defmethod find-component ((registry mcp-registry) (type (eql :tool)) name)
  "Find a tool by name"
  (gethash name (registry-tools registry)))

(defmethod find-component ((registry mcp-registry) (type (eql :prompt)) name)
  "Find a prompt by name"
  (gethash name (registry-prompts registry)))

(defmethod find-component ((registry mcp-registry) (type (eql :resource)) name)
  "Find a resource by name"
  (gethash name (registry-resources registry)))

(defmethod find-component ((registry mcp-registry) (type (eql :plugin)) name)
  "Find a plugin by name"
  (gethash name (registry-plugins registry)))

(defmethod list-components ((registry mcp-registry) (type (eql :tool)))
  "List all tools"
  (loop for tool being the hash-values of (registry-tools registry)
        collect tool))

(defmethod list-components ((registry mcp-registry) (type (eql :prompt)))
  "List all prompts"
  (loop for prompt being the hash-values of (registry-prompts registry)
        collect prompt))

(defmethod list-components ((registry mcp-registry) (type (eql :resource)))
  "List all resources"
  (loop for resource being the hash-values of (registry-resources registry)
        collect resource))

(defmethod list-components ((registry mcp-registry) (type (eql :resource-template)))
  "List all resource templates"
  (loop for template being the hash-values of (registry-resource-templates registry)
        collect template))

(defmethod component-exists-p ((registry mcp-registry) component-type name)
  "Check if a component exists"
  (not (null (find-component registry component-type name))))

;;; Global registry instance
(defvar *global-registry* (make-instance 'mcp-registry)
  "Global registry for all MCP components")

;;; Convenience macros for registration

(defmacro define-tool (name &body body)
  "Define and register a tool"
  `(register-component *global-registry* :tool ,name
                      (make-instance 'mcp-tool ,@body)))

(defmacro define-prompt (name &body body)
  "Define and register a prompt"
  `(register-component *global-registry* :prompt ,name
                      (make-instance 'prompt ,@body)))

(defmacro define-resource (name &body body)
  "Define and register a resource"
  `(register-component *global-registry* :resource ,name
                      (make-instance 'resource ,@body)))

;;; Bulk registration functions

(defun register-tools-from-template (template-data)
  "Register multiple tools from template data"
  (loop for tool-data in template-data
        for name = (gethash "name" tool-data)
        for tool = (create-tool-from-template tool-data)
        do (register-component *global-registry* :tool name tool)))

(defun register-prompts-from-template (template-data)
  "Register multiple prompts from template data"
  (loop for prompt-data in template-data
        for name = (gethash "name" prompt-data)
        for prompt = (create-prompt-from-template prompt-data)
        do (register-component *global-registry* :prompt name prompt)))

(defun register-resources-from-template (template-data)
  "Register multiple resources from template data"
  (loop for resource-data in template-data
        for uri = (gethash "uri" resource-data)
        for resource = (create-resource-from-template resource-data)
        do (register-component *global-registry* :resource uri resource)))

;;; Plugin system

(defclass mcp-plugin ()
  ((name :initarg :name :reader plugin-name :type string)
   (version :initarg :version :reader plugin-version :type string)
   (description :initarg :description :reader plugin-description :type string)
   (enabled-p :initform t :accessor plugin-enabled-p))
  (:documentation "Base class for MCP plugins"))

(defgeneric initialize-plugin (plugin registry)
  (:documentation "Initialize a plugin with the registry"))

(defgeneric shutdown-plugin (plugin)
  (:documentation "Cleanup when shutting down a plugin"))

(defgeneric plugin-components (plugin)
  (:documentation "Return components provided by this plugin")
  (:method ((plugin mcp-plugin)) '()))

;;; Example plugin implementation

(defclass tools-plugin (mcp-plugin)
  ((tools :initarg :tools :reader plugin-tools :initform '()))
  (:documentation "Plugin that provides additional tools"))

(defmethod initialize-plugin ((plugin tools-plugin) registry)
  "Register all tools provided by this plugin"
  (dolist (tool (plugin-tools plugin))
    (register-component registry :tool (tool-name tool) tool)))

(defmethod shutdown-plugin ((plugin tools-plugin))
  "Unregister all tools when shutting down"
  (dolist (tool (plugin-tools plugin))
    (unregister-component *global-registry* :tool (tool-name tool))))

;;; Registry management functions

(defun clear-registry (&optional (registry *global-registry*))
  "Clear all components from the registry"
  (clrhash (registry-tools registry))
  (clrhash (registry-prompts registry))
  (clrhash (registry-resources registry))
  (clrhash (registry-resource-templates registry))
  (clrhash (registry-plugins registry)))

(defun registry-stats (&optional (registry *global-registry*))
  "Return statistics about the registry"
  (list :tools (hash-table-count (registry-tools registry))
        :prompts (hash-table-count (registry-prompts registry))
        :resources (hash-table-count (registry-resources registry))
        :resource-templates (hash-table-count (registry-resource-templates registry))
        :plugins (hash-table-count (registry-plugins registry))))

(defun load-plugin (plugin-name plugin-instance &optional (registry *global-registry*))
  "Load and initialize a plugin"
  (register-component registry :plugin plugin-name plugin-instance)
  (initialize-plugin plugin-instance registry)
  plugin-instance)

(defun unload-plugin (plugin-name &optional (registry *global-registry*))
  "Shutdown and unload a plugin"
  (let ((plugin (find-component registry :plugin plugin-name)))
    (when plugin
      (shutdown-plugin plugin)
      (unregister-component registry :plugin plugin-name))))

;;; Integration with existing system

(defun get-available-tools-from-registry ()
  "Get all tools from the registry"
  (list-components *global-registry* :tool))

(defun get-available-prompts-from-registry ()
  "Get all prompts from the registry"
  (list-components *global-registry* :prompt))

(defun get-available-resources-from-registry ()
  "Get all resources from the registry"
  (list-components *global-registry* :resource))

(defun find-tool-in-registry (tool-name)
  "Find a tool by name in the registry"
  (find-component *global-registry* :tool tool-name))

(defun find-prompt-in-registry (prompt-name)
  "Find a prompt by name in the registry"
  (find-component *global-registry* :prompt prompt-name))

(defun find-resource-in-registry (resource-uri)
  "Find a resource by URI in the registry"
  (find-component *global-registry* :resource resource-uri))