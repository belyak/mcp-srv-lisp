;; =================================================================
;; Test Package Definition (tests/test-package.lisp)
;; =================================================================

(defpackage #:mcp-server-tests
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types
                #:json-rpc-request
                #:json-rpc-response
                #:json-rpc-error
                #:make-json-rpc-response
                #:make-json-rpc-error
                #:encode-to-json
                #:parse-from-json)
  (:import-from #:mcp-server.tools
                #:handle-tools-list
                #:handle-tool-call
                #:get-available-tools)
  (:import-from #:mcp-server.prompts
                #:handle-prompts-list
                #:handle-prompts-get
                #:get-available-prompts)
  (:import-from #:mcp-server.resources
                #:handle-resources-list
                #:handle-resource-read
                #:get-available-resources)
  (:import-from #:mcp-server.utilities
                #:handle-initialize
                #:handle-ping
                #:handle-connect
                #:handle-disconnect)
  (:export #:run-all-tests
           #:test-types
           #:test-tools
           #:test-prompts
           #:test-resources
           #:test-utilities))

(in-package #:mcp-server-tests)

;; Simple test framework
(defvar *test-results* nil)
(defvar *current-test-suite* nil)

(defmacro deftest (name &body body)
  `(defun ,name ()
     (let ((*current-test-suite* ,(string name)))
       (handler-case
           (progn ,@body
                  (push (list *current-test-suite* :pass nil) *test-results*)
                  (format t "✓ ~A~%" *current-test-suite*))
         (error (e)
           (push (list *current-test-suite* :fail e) *test-results*)
           (format t "✗ ~A: ~A~%" *current-test-suite* e))))))

(defmacro assert-equal (expected actual &optional description)
  `(unless (equal ,expected ,actual)
     (error "Assertion failed~@[ (~A)~]: expected ~A, got ~A"
            ,description ,expected ,actual)))

(defmacro assert-true (expr &optional description)
  `(unless ,expr
     (error "Assertion failed~@[ (~A)~]: expected true, got ~A"
            ,description ,expr)))

(defmacro assert-not-nil (expr &optional description)
  `(when (null ,expr)
     (error "Assertion failed~@[ (~A)~]: expected non-nil, got nil"
            ,description)))

(defun run-test-suite (tests)
  "Run a suite of tests"
  (setf *test-results* nil)
  (dolist (test tests)
    (funcall test))
  (let ((passed (count :pass *test-results* :key #'second))
        (failed (count :fail *test-results* :key #'second)))
    (format t "~%Test Results: ~A passed, ~A failed~%" passed failed)
    (when (> failed 0)
      (format t "~%Failed tests:~%")
      (dolist (result *test-results*)
        (when (eq (second result) :fail)
          (format t "  ~A: ~A~%" (first result) (third result)))))
    (= failed 0)))

