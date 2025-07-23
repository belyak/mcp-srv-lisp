;; ====================================================================
;; Templates (src/templates.lisp)
;; ====================================================================

(defpackage #:mcp-server.templates
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:mcp-server.types)
  (:export #:load-tools-template
           #:load-prompts-template
           #:load-resources-template))

(in-package #:mcp-server.templates)

;; Tools Template Data
(defparameter *tools-template*
  '[{
     "name": "get_current_time_in_city",
     "description": "Display current time in the city",
     "inputSchema": {
       "type": "object",
       "properties": {
         "city": {
           "type": "string",
           "description": "City name"
         }
       },
       "required": ["city"]
     }
   }])

;; Prompts Template Data
(defparameter *prompts-template*
  '[{
     "name": "current-time",
     "description": "Display current time in the city",
     "arguments": [{
       "name": "city",
       "description": "City name",
       "required": true
     }]
   },
   {
     "name": "analyze-code",
     "description": "Analyze code for potential improvements",
     "arguments": [{
       "name": "language",
       "description": "Programming language",
       "required": true
     }]
   }])

;; Resources Template Data
(defparameter *resources-template*
  '[{
     "uri": "file:///logs/app.log",
     "name": "Application Logs",
     "description": "application logs with timestamp, level, message",
     "mimeType": "text/plain"
   }])

(defun load-tools-template ()
  "Load tools from template data"
  (yason:parse *tools-template*))

(defun load-prompts-template ()
  "Load prompts from template data"
  (yason:parse *prompts-template*))

(defun load-resources-template ()
  "Load resources from template data"
  (yason:parse *resources-template*))
