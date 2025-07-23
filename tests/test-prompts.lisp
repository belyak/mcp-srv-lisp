;; =================================================================
;; Prompts Tests (tests/test-prompts.lisp)
;; =================================================================

(deftest test-prompts-list-handler
  (let ((result (handle-prompts-list nil)))
    (assert-not-nil result)
    (assert-not-nil (prompts-list result))
    (assert-equal 2 (length (prompts-list result)))
    (let ((prompt-names (mapcar #'prompt-name (prompts-list result))))
      (assert-true (member "current-time" prompt-names :test #'string=))
      (assert-true (member "analyze-code" prompt-names :test #'string=)))))

(deftest test-get-available-prompts
  (let ((prompts (get-available-prompts)))
    (assert-not-nil prompts)
    (assert-equal 2 (length prompts))))

(deftest test-prompts-get-current-time
  (let* ((params (alexandria:alist-hash-table '(("name" . "current-time")
                                                ("arguments" . (("city" . "Tokyo"))))
                                              :test 'equal))
         (result (handle-prompts-get params)))
    (assert-not-nil result)
    (assert-not-nil (prompt-result-description result))
    (assert-not-nil (prompt-result-messages result))))

(deftest test-prompts-get-missing-arguments
  (let* ((params (alexandria:alist-hash-table '(("name" . "current-time")) :test 'equal))
         (result (handle-prompts-get params)))
    (assert-not-nil result)
    (when (prompt-result-messages result)
      (let* ((message (first (prompt-result-messages result)))
             (content (msg-content message)))
        (assert-true (search "<missing>" (msg-content-text content)))))))

(deftest test-prompts-get-nonexistent
  (handler-case
      (progn
        (handle-prompts-get (alexandria:alist-hash-table '(("name" . "nonexistent")) :test 'equal))
        (error "Should have raised an error for nonexistent prompt"))
    (error (e)
      ;; Expected error
      (assert-true (search "not found" (format nil "~A" e))))))

(defun test-prompts ()
  "Run all prompts tests"
  (format t "~%Running Prompts Tests...~%")
  (run-test-suite '(test-prompts-list-handler
                    test-get-available-prompts
                    test-prompts-get-current-time
                    test-prompts-get-missing-arguments
                    test-prompts-get-nonexistent)))

