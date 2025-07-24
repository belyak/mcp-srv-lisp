;; ====================================================================
;; ASDF System Definition (mcp-server.asd) - FIXED VERSION
;; ====================================================================

(defsystem "mcp-server"
  :description "Model Context Protocol (MCP) Common Lisp server"
  :author "Andrei Beliak <andrei.beliak@yahoo.com>"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:serapeum
               #:yason
               #:bordeaux-threads
               #:usocket
               #:flexi-streams
               #:local-time
               #:quri
               #:cl-ppcre
               #:unix-opts
               #:trivial-signal
               #:closer-mop
               #:split-sequence)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "constants" :depends-on ("package"))
                 (:file "templates" :depends-on ("package"))  ; Move before types
                 (:file "types" :depends-on ("package" "constants"))
                 (:file "utilities" :depends-on ("package" "types" "constants"))
                 (:file "conditions" :depends-on ("package"))
                 (:file "hierarchy" :depends-on ("package" "types" "conditions"))
                 (:file "json-mixins" :depends-on ("package" "types"))
                 (:file "registry" :depends-on ("package" "types" "conditions"))
                 (:file "factory" :depends-on ("package" "types" "conditions"))
                 (:file "command-tools" :depends-on ("package" "types" "conditions"))
                 (:file "protocol" :depends-on ("package" "types" "utilities" "conditions"))
                 (:file "tools" :depends-on ("package" "types" "templates" "command-tools" "registry" "factory"))
                 (:file "prompts" :depends-on ("package" "types" "templates" "registry" "factory"))
                 (:file "resources" :depends-on ("package" "types" "templates" "registry" "factory"))
                 ;; (:file "oo-init" :depends-on ("package" "types" "templates" "hierarchy" "json-mixins" "registry" "factory" "command-tools" "protocol" "conditions" "tools" "prompts" "resources"))
                 (:file "main" :depends-on ("package" "types" "utilities" "protocol" "conditions" "tools" "prompts" "resources")))))
  :build-operation "program-op"
  :build-pathname "mcp-server"
  :entry-point "mcp-server:main")

;; Test system definition
(defsystem "mcp-server/tests"
  :depends-on ("mcp-server")
  :components ((:module "tests"
                :components
                ((:file "test-package")
                 (:file "test-types" :depends-on ("test-package"))
                 (:file "test-tools" :depends-on ("test-package"))
                 (:file "test-prompts" :depends-on ("test-package"))
                 (:file "test-resources" :depends-on ("test-package"))
                 (:file "test-utilities" :depends-on ("test-package")))))
  :perform (test-op (o c) (symbol-call :mcp-server-tests :run-all-tests)))