;; ====================================================================
;; Prompts Implementation (src/prompts.lisp)
;; ====================================================================

(in-package #:mcp-server.prompts)

(defvar *prompts-cache* nil
  "Cache for loaded prompts")

(defun load-prompts ()
  "Load prompts from template data"
  (unless *prompts-cache*
    (setf *prompts-cache* (load-prompts-template)))
  *prompts-cache*)

(defun convert-prompt-from-json (prompt-json)
  "Convert JSON prompt definition to prompt object"
  (let ((arguments (when (gethash "arguments" prompt-json)
                     (map 'list (lambda (arg)
                                  (make-instance 'prompt-argument
                                                 :name (gethash "name" arg)
                                                 :description (gethash "description" arg)
                                                 :required (gethash "required" arg)))
                          (gethash "arguments" prompt-json)))))
    (make-instance 'prompt
                   :name (gethash "name" prompt-json)
                   :description (gethash "description" prompt-json)
                   :arguments arguments)))

(defun get-available-prompts ()
  "Get list of available prompts"
  (map 'list #'convert-prompt-from-json (load-prompts)))

(defun handle-prompts-list (request)
  "Handle prompts/list request"
  (declare (ignore request))
  (make-instance 'list-prompts-result
                 :prompts (get-available-prompts)))

(defun find-prompt-by-name (name)
  "Find prompt by name"
  (find name (get-available-prompts) :key #'prompt-name :test #'string=))

(defun handle-prompts-get (request)
  "Handle prompts/get request"
  (let* ((name (gethash "name" request))
         (arguments (gethash "arguments" request))
         (prompt (find-prompt-by-name name)))
    
    (unless prompt
      (error "Prompt not found: ~A" name))
    
    (let ((messages (when (prompt-arguments prompt)
                      (list (make-instance 'prompt-message
                                           :role "user"
                                           :content (make-instance 'prompt-message-content
                                                                   :type "text"
                                                                   :text (format nil "~A: ~A"
                                                                                 (arg-name (first (prompt-arguments prompt)))
                                                                                 (or (and arguments 
                                                                                          (gethash (arg-name (first (prompt-arguments prompt))) arguments))
                                                                                     "<missing>"))))))))
      
      (make-instance 'prompt-result
                     :description (or (prompt-description prompt) "")
                     :messages messages))))

