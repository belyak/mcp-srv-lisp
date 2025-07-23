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
  (:export #:main
           #:start-server
           #:*debug-mode*))
