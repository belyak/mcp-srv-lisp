;;;; Command Pattern for Tool Execution
;;;; This implements OOP improvement #5: Command Pattern for Tool Execution

(in-package :mcp-server)

;;; Abstract tool class with command pattern
(defclass mcp-tool ()
  ((name :initarg :name :reader tool-name :type string)
   (description :initarg :description :reader tool-description :type string)
   (input-schema :initarg :input-schema :reader tool-input-schema))
  (:documentation "Abstract base class for all MCP tools"))

;;; Tool execution protocol
(defgeneric execute-tool (tool params)
  (:documentation "Execute tool with given parameters"))

(defgeneric validate-params (tool params)
  (:documentation "Validate parameters against tool schema"))

(defgeneric tool-category (tool)
  (:documentation "Return the category of the tool")
  (:method ((tool mcp-tool)) "general"))

;;; Parameter validation implementation
(defmethod validate-params ((tool mcp-tool) params)
  "Default parameter validation using schema"
  (let* ((schema (tool-input-schema tool))
         (properties (when (typep schema 'tool-input-schema)
                       (schema-properties schema)))
         (required (when (typep schema 'tool-input-schema)
                     (schema-required schema))))
    ;; Check required parameters
    (dolist (req required)
      (unless (gethash req params)
        (error 'missing-required-parameter 
               :parameter req
               :message (format nil "Missing required parameter: ~A" req))))
    ;; Note: Property validation is limited since tool-input-schema-property
    ;; doesn't store property names - this would need schema redesign
    (when properties
      ;; For now, just validate that properties is a list
      (unless (listp properties)
        (error 'invalid-params-error 
               :message "Tool schema properties must be a list")))
    t))

(defun validate-parameter-type (name value expected-type)
  "Validate that a parameter value matches expected type"
  (let ((valid-p
         (cond
           ((string= expected-type "string") (stringp value))
           ((string= expected-type "number") (numberp value))
           ((string= expected-type "boolean") (or (eq value t) (eq value nil)))
           ((string= expected-type "array") (or (listp value) (vectorp value)))
           ((string= expected-type "object") (hash-table-p value))
           (t t)))) ; Unknown types pass validation
    (unless valid-p
      (error 'invalid-parameter-type
             :parameter name
             :expected-type expected-type
             :actual-value value))))

;;; Concrete tool implementations

(defclass time-tool (mcp-tool)
  ()
  (:default-initargs
   :name "get_current_time_in_city"
   :description "Get the current time in a specific city"
   :input-schema (make-instance 'mcp-server.types:tool-input-schema
                               :type "object"
                               :properties (list (make-instance 'mcp-server.types:tool-input-schema-property
                                                               :type "string"
                                                               :description "The city name"
                                                               :enum-values '("Tokyo" "London" "New York")))
                               :required '("city"))))

(defmethod execute-tool ((tool time-tool) params)
  "Execute the time tool"
  (validate-params tool params)
  (let* ((city (gethash "city" params "Unknown"))
         (current-time (local-time:now))
         (time-string (local-time:format-rfc1123-timestring nil current-time)))
    (make-instance 'call-tool-result
                   :content (list (make-instance 'call-tool-result-content
                                               :type "text"
                                               :text (format nil "Now: ~A (in ~A)!" 
                                                           time-string city)))
                   :is-error nil)))

(defmethod tool-category ((tool time-tool))
  "datetime")

;;; Tool with complex validation
(defclass calculator-tool (mcp-tool)
  ()
  (:default-initargs
   :name "calculator"
   :description "Perform basic arithmetic operations"
   :input-schema (make-instance 'mcp-server.types:tool-input-schema
                               :type "object"
                               :properties (list 
                                          (make-instance 'mcp-server.types:tool-input-schema-property
                                                        :type "string"
                                                        :description "The operation to perform"
                                                        :enum-values '("add" "subtract" "multiply" "divide"))
                                          (make-instance 'mcp-server.types:tool-input-schema-property
                                                        :type "number"
                                                        :description "First operand")
                                          (make-instance 'mcp-server.types:tool-input-schema-property
                                                        :type "number"
                                                        :description "Second operand"))
                               :required '("operation" "a" "b"))))

(defmethod execute-tool ((tool calculator-tool) params)
  "Execute calculator operations"
  (validate-params tool params)
  (let ((op (gethash "operation" params))
        (a (gethash "a" params))
        (b (gethash "b" params)))
    (handler-case
        (let ((result
               (cond
                 ((string= op "add") (+ a b))
                 ((string= op "subtract") (- a b))
                 ((string= op "multiply") (* a b))
                 ((string= op "divide") 
                  (if (zerop b)
                      (error "Division by zero")
                      (/ a b)))
                 (t (error "Unknown operation: ~A" op)))))
          (make-instance 'call-tool-result
                         :content (list (make-instance 'call-tool-result-content
                                                     :type "text"
                                                     :text (format nil "Result: ~A" result)))
                         :is-error nil))
      (error (e)
        (make-instance 'call-tool-result
                       :content (list (make-instance 'call-tool-result-content
                                                   :type "text"
                                                   :text (format nil "Error: ~A" e)))
                       :is-error t)))))

(defmethod tool-category ((tool calculator-tool))
  "math")

;;; Tool registry using command pattern
(defclass tool-registry ()
  ((tools :initform (make-hash-table :test 'equal) :accessor registry-tools))
  (:documentation "Registry for managing tool instances"))

(defgeneric register-tool (registry tool)
  (:documentation "Register a tool in the registry"))

(defgeneric find-tool (registry name)
  (:documentation "Find a tool by name"))

(defgeneric list-tools (registry)
  (:documentation "List all registered tools"))

(defmethod register-tool ((registry tool-registry) (tool mcp-tool))
  "Register a tool instance"
  (setf (gethash (tool-name tool) (registry-tools registry)) tool))

(defmethod find-tool ((registry tool-registry) name)
  "Find a tool by name"
  (gethash name (registry-tools registry)))

(defmethod list-tools ((registry tool-registry))
  "List all tools"
  (loop for tool being the hash-values of (registry-tools registry)
        collect tool))

;;; Global tool registry
(defvar *tool-registry* (make-instance 'tool-registry)
  "Global registry for all tools")

;;; Tool registration helpers
(defun register-default-tools ()
  "Register all default tools"
  (register-tool *tool-registry* (make-instance 'time-tool))
  (register-tool *tool-registry* (make-instance 'calculator-tool)))

;;; Enhanced tool execution with registry
(defun execute-tool-by-name (tool-name params)
  "Execute a tool by name using the registry"
  (let ((tool (find-tool *tool-registry* tool-name)))
    (if tool
        (execute-tool tool params)
        (error 'tool-not-found-error :tool-name tool-name))))

;;; Tool creation from templates (backward compatibility)
(defun create-tool-from-template (template)
  "Create a tool instance from a template hash table"
  (let ((name (gethash "name" template))
        (description (gethash "description" template))
        (input-schema-data (gethash "inputSchema" template)))
    ;; For now, create a generic tool - this could be extended
    ;; to create specific tool types based on the name
    (make-instance 'mcp-tool
                   :name name
                   :description description
                   :input-schema (parse-tool-input-schema input-schema-data))))

(defun parse-tool-input-schema (schema-data)
  "Parse input schema from JSON data"
  (when schema-data
    (make-instance 'mcp-server.types:tool-input-schema
                   :type (gethash "type" schema-data "object")
                   :properties (when (gethash "properties" schema-data)
                                (mapcar #'parse-schema-property 
                                      (coerce (gethash "properties" schema-data '()) 'list)))
                   :required (coerce (gethash "required" schema-data '()) 'list))))

(defun parse-schema-property (prop-data)
  "Parse a schema property from JSON data"
  (make-instance 'mcp-server.types:tool-input-schema-property
                 :type (gethash "type" prop-data)
                 :description (gethash "description" prop-data)
                 :enum-values (when (gethash "enum" prop-data)
                               (coerce (gethash "enum" prop-data) 'list))))

;;; Integration with existing system
(defun get-available-tools-v2 ()
  "Get all available tools from the registry"
  (list-tools *tool-registry*))

(defun handle-tool-call-v2 (tool-name params)
  "Handle tool call using command pattern"
  (execute-tool-by-name tool-name params))

;;; Tool composition and chaining
(defclass composite-tool (mcp-tool)
  ((tools :initarg :tools :accessor composite-tools :initform '()))
  (:documentation "A tool that executes multiple tools in sequence"))

(defmethod execute-tool ((tool composite-tool) params)
  "Execute all tools in sequence, passing results forward"
  (let ((result params))
    (dolist (sub-tool (composite-tools tool))
      (let ((tool-result (execute-tool sub-tool result)))
        ;; Extract the result text to pass to next tool
        (when (and (typep tool-result 'call-tool-result)
                   (not (result-is-error tool-result)))
          (let ((content (first (result-content tool-result))))
            (when content
              (setf result (alexandria:alist-hash-table
                           `(("input" . ,(content-text content)))
                           :test 'equal)))))))
    result))

;;; Initialize the tool system
(register-default-tools)