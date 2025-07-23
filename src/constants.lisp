;; ====================================================================
;; Constants (src/constants.lisp)
;; ====================================================================

(defpackage #:mcp-server.constants
  (:use #:cl)
  (:export #:+jsonrpc-version+
           #:+protocol-version+
           #:+server-name+
           #:+server-version+))

(in-package #:mcp-server.constants)

(defconstant +jsonrpc-version+ "2.0")
(defconstant +protocol-version+ "2024-11-05")
(defconstant +server-name+ "mcp-lisp-server")
(defconstant +server-version+ "0.1.0")
