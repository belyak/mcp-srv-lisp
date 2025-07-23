;; =================================================================
;; Tools Tests (tests/test-tools.lisp)
;; =================================================================

(deftest test-tools-list-handler
  (let ((result (handle-tools-list nil)))
    (assert-not-nil result)
    (assert-not-nil (tools-list result))
    (assert-equal 1 (length (tools-list result)))
    (let ((tool (first (tools-list result))))
      (assert-equal "get_current_time_in_city" (tool-name tool))
      (assert-not-nil (tool-description tool))
      (assert-not-nil (tool-input-schema tool)))))

(deftest test-get-available-tools
  (let ((tools (get-available-tools)))
    (assert-not-nil tools)
    (assert-equal 1 (length tools))
    (let ((tool (first tools)))
      (assert-equal "get_current_time_in_city" (tool-name tool)))))

(deftest test-current-time-tool-call
  (let* ((params (alexandria:alist-hash-table '(("city" . "New York")) :test 'equal))
         (result (handle-tool-call "get_current_time_in_city" params)))
    (assert-not-nil result)
    (assert-not-nil (result-content result))
    (assert-equal nil (result-is-error result))
    (let ((content (first (result-content result))))
      (assert-equal "text" (content-type content))
      (assert-true (search "Now:" (content-text content))))))

(deftest test-invalid-tool-call
  (handler-case
      (progn
        (handle-tool-call "nonexistent_tool" (make-hash-table))
        (error "Should have raised an error for invalid tool"))
    (error (e)
      ;; Expected error
      (assert-true (search "Unknown tool" (format nil "~A" e))))))

(defun test-tools ()
  "Run all tools tests"
  (format t "~%Running Tools Tests...~%")
  (run-test-suite '(test-tools-list-handler
                    test-get-available-tools
                    test-current-time-tool-call
                    test-invalid-tool-call)))

