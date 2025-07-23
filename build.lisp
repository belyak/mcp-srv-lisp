(require 'asdf)
;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ensure we can find our system
(push (uiop:getcwd) asdf:*central-registry*)

;; Load system with dependencies
#+sbcl
(handler-case
    (ql:quickload :mcp-server :silent t)
  (error (e)
    (format *error-output* "Error loading system: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Build executable
#+sbcl
(handler-case
    (sb-ext:save-lisp-and-die "mcp-server"
                              :toplevel #'mcp-server:main
                              :executable t
                              :compression t
                              :save-runtime-options t)
  (error (e)
    (format *error-output* "Error building executable: ~A~%" e)
    (sb-ext:exit :code 1)))

#+ccl
(handler-case
    (ccl:save-application "mcp-server"
                          :toplevel-function #'mcp-server:main
                          :prepend-kernel t)
  (error (e)
    (format *error-output* "Error building executable: ~A~%" e)
    (ccl:quit 1)))

#+ecl
(handler-case
    (asdf:make-build :mcp-server
                     :type :program
                     :name "mcp-server"
                     :epilogue-code '(mcp-server:main))
  (error (e)
    (format *error-output* "Error building executable: ~A~%" e)
    (si:quit 1)))

#-(or sbcl ccl ecl)
(progn
  (format *error-output* "Unsupported Lisp implementation~%")
  (quit 1))
