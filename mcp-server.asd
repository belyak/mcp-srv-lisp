;; ====================================================================
;; ASDF System Definition (mcp-server.asd)
;; ====================================================================

(defsystem "mcp-server"
  :description "Model Context Protocol (MCP) Common Lisp stdioserver"
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
               #:trivial-signal)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "constants" :depends-on ("package"))
                 (:file "types" :depends-on ("package" "constants"))
                 (:file "utilities" :depends-on ("package" "types" "constants"))
                 (:file "templates" :depends-on ("package" "types"))
                 (:file "tools" :depends-on ("package" "types" "templates"))
                 (:file "prompts" :depends-on ("package" "types" "templates"))
                 (:file "resources" :depends-on ("package" "types" "templates"))
                 (:file "main" :depends-on ("package" "types" "utilities" "tools" "prompts" "resources")))))
  :build-operation "program-op"
  :build-pathname "mcp-server"
  :entry-point "mcp-server:main")