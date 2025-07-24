;; ====================================================================
;; Package Definition (src/package.lisp)
;; ====================================================================

(defpackage #:mcp-server
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:yason
                #:encode
                #:parse
                #:with-output-to-string*
                #:with-object
                #:encode-object-element
                #:encode-array-element)
  (:import-from #:local-time
                #:now
                #:format-rfc1123-timestring)
  (:import-from #:quri
                #:uri
                #:render-uri)
  (:import-from #:unix-opts
                #:define-opts
                #:get-opts)
  (:import-from #:trivial-signal
                #:signal-handler-bind)
  (:export #:main              ; Fix: Export main function
           #:start-server
           #:*debug-mode*
           ;; Protocol exports
           #:handle-request
           #:mcp-request
           #:create-request-from-json
           #:process-request-oop
           ;; Condition exports
           #:mcp-error
           #:method-not-found-error
           #:invalid-params-error
           #:missing-required-parameter
           #:tool-not-found-error
           #:prompt-not-found-error
           #:resource-not-found-error
           ;; Hierarchy exports
           #:mcp-list-result
           #:mcp-content
           #:mcp-named-entity
           #:mcp-identifiable
           #:to-json-representation
           #:validate-entity
           #:entity-type
           ;; JSON Mixin exports
           #:json-serializable
           #:auto-json-serializable
           #:selective-json-serializable
           #:cached-json-serializable
           #:to-json-alist
           #:from-json-alist
           #:json-slots
           #:define-json-class
           ;; Registry exports
           #:mcp-registry
           #:*global-registry*
           #:register-component
           #:unregister-component
           #:find-component
           #:list-components
           #:component-exists-p
           #:get-available-tools-from-registry
           #:get-available-prompts-from-registry
           #:get-available-resources-from-registry
           #:find-tool-in-registry
           #:handle-tools-list-enhanced
           #:handle-tool-call-enhanced
           #:handle-prompts-list-enhanced
           #:handle-resources-list-enhanced
           #:mcp-plugin
           #:initialize-plugin
           #:shutdown-plugin
           #:define-tool
           #:define-prompt
           #:define-resource
           ;; Factory exports
           #:create-from-json
           #:create-from-params
           #:mcp-factory
           #:create-object
           #:register-type
           #:*request-factory*
           #:*tool-factory*
           #:create-tool-from-template
           #:create-prompt-from-template
           #:create-resource-from-template
           ;; Command tools exports
           #:mcp-tool
           #:execute-tool
           #:validate-params
           #:tool-category
           #:time-tool
           #:calculator-tool
           #:tool-registry
           #:*tool-registry*
           #:execute-tool-by-name))

;; Sub-packages for modular organization
(defpackage #:mcp-server.constants
  (:use #:cl)
  (:export #:+jsonrpc-version+
           #:+protocol-version+
           #:+server-name+
           #:+server-version+))

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
           #:list-resource-templates-result
           #:read-resource-result
           #:connect-result
           #:disconnect-result
           #:execute-line-result
           #:empty-result
           #:cancelled-notification
           #:make-json-rpc-response
           #:make-json-rpc-error
           #:encode-to-json
           #:parse-from-json
           ;; Accessors with fixed names
           #:request-id #:request-method #:request-params
           #:response-id #:response-result
           #:error-id #:error-code #:error-message #:error-data
           #:tool-name #:tool-description #:tool-input-schema
           #:schema-type #:schema-properties #:schema-required
           #:prop-type #:prop-description #:prop-enum-values
           #:result-content #:result-is-error
           #:content-type #:content-text #:content-data #:content-mime-type
           #:tools-list #:tools-next-cursor
           #:prompt-name #:prompt-description #:prompt-arguments
           #:arg-name #:arg-description #:arg-required
           #:msg-role #:msg-content
           #:msg-content-type #:msg-content-text
           #:prompt-result-description #:prompt-result-messages
           #:prompts-list #:prompts-next-cursor
           #:resource-uri #:resource-name #:resource-description #:resource-mime-type
           #:content-uri #:content-text #:content-blob
           #:resources-list #:resources-next-cursor
           #:resource-content
           #:connection-id #:disconnect-success
           #:exec-output #:exec-is-error
           #:cancel-request-id #:cancel-reason
           #:impl-name #:impl-version
           #:client-experimental #:client-roots #:client-sampling
           #:server-experimental #:server-prompts #:server-resources 
           #:server-tools #:server-roots #:server-sampling #:server-logging
           #:init-protocol-version #:init-capabilities #:init-client-info
           #:result-protocol-version #:result-capabilities 
           #:result-server-info #:result-instructions))

(defpackage #:mcp-server.templates
  (:use #:cl #:alexandria #:serapeum)
  (:export #:load-tools-template
           #:load-prompts-template
           #:load-resources-template))

(defpackage #:mcp-server.utilities
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:json-rpc-response
                #:json-rpc-error
                #:make-json-rpc-response
                #:make-json-rpc-error
                #:initialize-result
                #:server-capabilities
                #:implementation
                #:empty-result
                #:connect-result
                #:disconnect-result)
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
           #:handle-completion
           #:graceful-shutdown
           #:notify))

(defpackage #:mcp-server.tools
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:list-tools-result
                #:tool
                #:call-tool-result
                #:call-tool-result-content
                #:tool-input-schema
                #:tool-input-schema-property)
  (:import-from #:mcp-server.templates
                #:load-tools-template)
  (:import-from #:local-time
                #:now
                #:format-rfc1123-timestring)
  (:export #:handle-tools-list
           #:handle-tool-call
           #:get-available-tools))

(defpackage #:mcp-server.prompts
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:list-prompts-result
                #:prompt
                #:prompt-result
                #:prompt-name
                #:prompt-description
                #:prompt-arguments
                #:prompt-argument
                #:arg-name
                #:prompt-message
                #:prompt-message-content)
  (:import-from #:mcp-server.templates
                #:load-prompts-template)
  (:export #:handle-prompts-list
           #:handle-prompts-get
           #:get-available-prompts))

(defpackage #:mcp-server.resources
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:list-resources-result
                #:resource
                #:read-resource-result
                #:resource-content)
  (:import-from #:mcp-server.templates
                #:load-resources-template)
  (:export #:handle-resources-list
           #:handle-resource-read
           #:handle-resources-read
           #:get-available-resources
           #:get-available-resource-templates
           #:handle-resources-subscribe
           #:handle-resources-unsubscribe))

(defpackage #:mcp-server.main
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:json-rpc-request
                #:json-rpc-response
                #:json-rpc-error
                #:make-json-rpc-response
                #:make-json-rpc-error
                #:encode-to-json)
  (:import-from #:mcp-server.utilities
                #:handle-initialize
                #:handle-ping
                #:handle-connect
                #:handle-disconnect
                #:handle-execute-line
                #:handle-logging-set-level
                #:handle-roots-list
                #:handle-cancelled-notification
                #:graceful-shutdown)
  (:import-from #:mcp-server.tools
                #:handle-tools-list
                #:handle-tool-call)
  (:import-from #:mcp-server.prompts
                #:handle-prompts-list
                #:handle-prompts-get)
  (:import-from #:mcp-server.resources
                #:handle-resources-list
                #:handle-resource-read)
  (:import-from #:mcp-server
                #:process-request-oop)
  (:import-from #:unix-opts)
  (:import-from #:trivial-signal)
  (:export #:main #:start-server))