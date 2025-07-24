;; =================================================================
;; Type System Tests (tests/test-types.lisp)
;; =================================================================

(in-package #:mcp-server-tests)

(deftest test-json-rpc-response-creation
  (let ((response (make-json-rpc-response 123 "test-result")))
    (assert-equal 123 (response-id response))
    (assert-equal "test-result" (response-result response))))

(deftest test-json-rpc-error-creation
  (let ((error (make-json-rpc-error 456 -32600 "Invalid Request")))
    (assert-equal 456 (error-id error))
    (assert-equal -32600 (error-code error))
    (assert-equal "Invalid Request" (error-message error))))

(deftest test-json-rpc-response-encoding
  (let* ((response (make-json-rpc-response 1 "success"))
         (json (encode-to-json response)))
    (assert-not-nil json)
    (assert-true (search "\"jsonrpc\":\"2.0\"" json))
    (assert-true (search "\"id\":1" json))
    (assert-true (search "\"result\":\"success\"" json))))

(deftest test-json-parsing
  (let* ((json-str "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}")
         (parsed (parse-from-json json-str 'json-rpc-request)))
    (assert-not-nil parsed)))

(defun test-types ()
  "Run all type system tests"
  (format t "~%Running Type System Tests...~%")
  (run-test-suite '(test-json-rpc-response-creation
                    test-json-rpc-error-creation
                    test-json-rpc-response-encoding
                    test-json-parsing)))

