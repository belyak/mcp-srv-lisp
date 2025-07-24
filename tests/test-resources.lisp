;; =================================================================
;; Resources Tests (tests/test-resources.lisp)
;; =================================================================

(in-package #:mcp-server-tests)

(deftest test-resources-list-handler
  (let ((result (handle-resources-list nil)))
    (assert-not-nil result)
    (assert-not-nil (resources-list result))
    (assert-equal 1 (length (resources-list result)))
    (let ((resource (first (resources-list result))))
      (assert-equal "Application Logs" (resource-name resource))
      (assert-not-nil (resource-uri resource)))))

(deftest test-get-available-resources
  (let ((resources (get-available-resources)))
    (assert-not-nil resources)
    (assert-equal 1 (length resources))))

(deftest test-resource-read
  (let* ((params (alexandria:alist-hash-table '(("uri" . "file:///logs/app.log")) :test 'equal))
         (result (handle-resource-read params)))
    (assert-not-nil result)
    (assert-not-nil (resource-content result))
    (let ((content (resource-content result)))
      (assert-equal "file:///logs/app.log" (content-uri content))
      (assert-equal "text/plain" (content-mime-type content))
      (assert-not-nil (content-text content)))))

(deftest test-resource-read-different-uri
  (let* ((params (alexandria:alist-hash-table '(("uri" . "file:///other/path.txt")) :test 'equal))
         (result (handle-resource-read params)))
    (assert-not-nil result)
    (let ((content (resource-content result)))
      (assert-equal "file:///other/path.txt" (content-uri content)))))

(defun test-resources ()
  "Run all resources tests"
  (format t "~%Running Resources Tests...~%")
  (run-test-suite '(test-resources-list-handler
                    test-get-available-resources
                    test-resource-read
                    test-resource-read-different-uri)))
