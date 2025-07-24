;; =================================================================
;; Utilities Tests (tests/test-utilities.lisp)
;; =================================================================

(in-package #:mcp-server-tests)

(deftest test-initialize-handler
  (let* ((params (alexandria:alist-hash-table '(("protocolVersion" . "2024-11-05")
                                                ("capabilities" . nil)
                                                ("clientInfo" . (("name" . "test-client")
                                                                ("version" . "1.0.0"))))
                                              :test 'equal))
         (result (handle-initialize params)))
    (assert-not-nil result)
    (assert-not-nil (result-protocol-version result))
    (assert-not-nil (result-server-info result))
    (assert-not-nil (result-capabilities result))))

(deftest test-ping-handler
  (let ((result (handle-ping nil)))
    (assert-not-nil result)))

(deftest test-connect-handler
  (let ((result (handle-connect nil)))
    (assert-not-nil result)
    (assert-equal "fake_connection_id" (connection-id result))))

(deftest test-disconnect-handler
  (let ((result (handle-disconnect nil)))
    (assert-not-nil result)
    (assert-true (disconnect-success result))))

(defun test-utilities ()
  "Run all utilities tests"
  (format t "~%Running Utilities Tests...~%")
  (run-test-suite '(test-initialize-handler
                    test-ping-handler
                    test-connect-handler
                    test-disconnect-handler)))

;; =================================================================
;; Integration Tests
;; =================================================================

(deftest test-full-tool-workflow
  "Test complete tool workflow from list to call"
  (let ((tools-result (handle-tools-list nil)))
    (assert-not-nil tools-result)
    (let* ((tool (first (tools-list tools-result)))
           (tool-name (tool-name tool))
           (params (alexandria:alist-hash-table '(("city" . "London")) :test 'equal))
           (call-result (handle-tool-call tool-name params)))
      (assert-not-nil call-result)
      (assert-equal nil (result-is-error call-result)))))

(deftest test-full-prompt-workflow
  "Test complete prompt workflow from list to get"
  (let ((prompts-result (handle-prompts-list nil)))
    (assert-not-nil prompts-result)
    (let* ((prompt (first (prompts-list prompts-result)))
           (prompt-name (prompt-name prompt))
           (params (alexandria:alist-hash-table `(("name" . ,prompt-name)
                                                  ("arguments" . ,(alexandria:alist-hash-table '(("city" . "Paris")) :test 'equal)))
                                                :test 'equal))
           (get-result (handle-prompts-get params)))
      (assert-not-nil get-result)
      (assert-not-nil (prompt-result-description get-result)))))

(deftest test-full-resource-workflow
  "Test complete resource workflow from list to read"
  (let ((resources-result (handle-resources-list nil)))
    (assert-not-nil resources-result)
    (let* ((resource (first (resources-list resources-result)))
           (resource-uri (resource-uri resource))
           (params (alexandria:alist-hash-table `(("uri" . ,resource-uri)) :test 'equal))
           (read-result (handle-resource-read params)))
      (assert-not-nil read-result)
      (assert-not-nil (resource-content read-result)))))

(defun test-integration ()
  "Run all integration tests"
  (format t "~%Running Integration Tests...~%")
  (run-test-suite '(test-full-tool-workflow
                    test-full-prompt-workflow
                    test-full-resource-workflow)))

;; =================================================================
;; Main Test Runner
;; =================================================================

(defun run-all-tests ()
  "Run all test suites"
  (format t "~%========================================~%")
  (format t "MCP Common Lisp Server Test Suite~%")
  (format t "========================================~%")
  
  (let ((all-passed t))
    (setf all-passed (and all-passed (test-types)))
    (setf all-passed (and all-passed (test-tools)))
    (setf all-passed (and all-passed (test-prompts)))
    (setf all-passed (and all-passed (test-resources)))
    (setf all-passed (and all-passed (test-utilities)))
    (setf all-passed (and all-passed (test-integration)))
    
    (format t "~%========================================~%")
    (if all-passed
        (format t "✅ ALL TESTS PASSED~%")
        (format t "❌ SOME TESTS FAILED~%"))
    (format t "========================================~%")
    
    all-passed))

;; =================================================================
;; Performance Tests
;; =================================================================

(defun benchmark-json-encoding ()
  "Benchmark JSON encoding performance"
  (format t "~%Running JSON encoding benchmark...~%")
  (let ((response (make-json-rpc-response 1 "test-result"))
        (iterations 10000)
        (start-time (get-internal-real-time)))
    
    (dotimes (i iterations)
      (encode-to-json response))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-sec (/ iterations elapsed)))
      (format t "JSON encoding: ~A operations in ~,3F seconds (~,0F ops/sec)~%"
              iterations elapsed ops-per-sec))))

(defun benchmark-tool-calls ()
  "Benchmark tool call performance"
  (format t "~%Running tool call benchmark...~%")
  (let ((params (alexandria:alist-hash-table '(("city" . "TestCity")) :test 'equal))
        (iterations 1000)
        (start-time (get-internal-real-time)))
    
    (dotimes (i iterations)
      (handle-tool-call "get_current_time_in_city" params))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-sec (/ iterations elapsed)))
      (format t "Tool calls: ~A operations in ~,3F seconds (~,0F ops/sec)~%"
              iterations elapsed ops-per-sec))))

(defun run-benchmarks ()
  "Run performance benchmarks"
  (format t "~%========================================~%")
  (format t "MCP Common Lisp Server Benchmarks~%")
  (format t "========================================~%")
  
  (benchmark-json-encoding)
  (benchmark-tool-calls)
  
  (format t "~%========================================~%")
  (format t "Benchmarks completed~%")
  (format t "========================================~%"))

;; =================================================================
;; ASDF Test Integration
;; =================================================================

;; This would be added to the main .asd file:
;;
;; (defsystem "mcp-server/tests"
;;   :depends-on ("mcp-server")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "test-package")
;;                  (:file "test-types" :depends-on ("test-package"))
;;                  (:file "test-tools" :depends-on ("test-package"))
;;                  (:file "test-prompts" :depends-on ("test-package"))
;;                  (:file "test-resources" :depends-on ("test-package"))
;;                  (:file "test-utilities" :depends-on ("test-package")))))
;;   :perform (test-op (o c) (symbol-call :mcp-server-tests :run-all-tests)))

;; =================================================================
;; Test Data and Fixtures
;; =================================================================

(defparameter *test-tool-params*
  (alexandria:alist-hash-table '(("city" . "Test City")) :test 'equal)
  "Test parameters for tool calls")

(defparameter *test-prompt-params*
  (alexandria:alist-hash-table '(("name" . "current-time")
                                 ("arguments" . (("city" . "Test City"))))
                               :test 'equal)
  "Test parameters for prompt calls")

(defparameter *test-resource-params*
  (alexandria:alist-hash-table '(("uri" . "file:///test/resource.txt")) :test 'equal)
  "Test parameters for resource calls")

;; =================================================================
;; Mock Data for Testing
;; =================================================================

(defun create-mock-initialize-request ()
  "Create a mock initialize request for testing"
  (alexandria:alist-hash-table
   '(("protocolVersion" . "2024-11-05")
     ("capabilities" . (("experimental" . nil)))
     ("clientInfo" . (("name" . "test-client") ("version" . "1.0.0"))))
   :test 'equal))

(defun create-mock-tool-call-request (tool-name args)
  "Create a mock tool call request for testing"
  (alexandria:alist-hash-table
   `(("name" . ,tool-name)
     ("arguments" . ,args))
   :test 'equal))

;; =================================================================
;; Helper Functions for Testing
;; =================================================================

(defun hash-table-equal (ht1 ht2)
  "Compare two hash tables for equality"
  (and (= (hash-table-count ht1) (hash-table-count ht2))
       (loop for key being the hash-keys of ht1
             always (equal (gethash key ht1) (gethash key ht2)))))

(defun assert-hash-table-has-key (ht key &optional description)
  "Assert that hash table has the specified key"
  (unless (nth-value 1 (gethash key ht))
    (error "Assertion failed~@[ (~A)~]: hash table missing key ~A" description key)))

(defun assert-string-contains (haystack needle &optional description)
  "Assert that haystack contains needle"
  (unless (search needle haystack)
    (error "Assertion failed~@[ (~A)~]: ~S does not contain ~S" description haystack needle)))

;; Export test functions for external use
(export '(run-all-tests
          run-benchmarks
          test-types
          test-tools
          test-prompts
          test-resources
          test-utilities
          test-integration) :mcp-server-tests)