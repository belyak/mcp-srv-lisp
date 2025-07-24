;; ====================================================================
;; Constants (src/constants.lisp)
;; ====================================================================

(in-package #:mcp-server.constants)

(defmacro define-constant (name value &optional doc)
  "Define a constant, handling redefinition gracefully"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +jsonrpc-version+ "2.0")
(define-constant +protocol-version+ "2024-11-05")
(define-constant +server-name+ "mcp-lisp-server")
(define-constant +server-version+ "0.1.0")
