;;; ob-clojurescript.el --- org-babel functions for clojurescript evaluation

;; Copyright (C) Andrew Cowper

;; Author: Andrew Cowper
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is a minimally functionaly plugin to allow ClojureScript
;; to be evaluatd by Org Babel. It was cobbled together from the Clojure
;; babel plugin shipped with Org. It is very basic and does not implement
;; everything that Babel might ask it to do.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

(defun clojurescript-mode () (clojure-mode))

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("clojurescript" . "cljs"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:clojurescript '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:clojurescript' function below.
(defun org-babel-expand-body:clojurescript (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (org-babel-trim
     (if (> (length vars) 0)
	 (concat "(let ["
		 (mapconcat
		  (lambda (var)
		    (format "%S (quote %S)" (car var) (org-babel-clojurescript-var-to-clojurescript (cdr var))))
		  vars
		  "\n      ")
		 "]\n" body ")")
       body))))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:clojurescript (body params)
  "Execute a block of Clojurescript code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Clojurescript source code block")
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
         (result-params (cdr (assoc :result-params params)))
	 (print-level nil)
	 (print-length nil)
	 (body (org-babel-trim
		(if (> (length vars) 0)
		    (concat "(let ["
			    (mapconcat
			     (lambda (var)
			       (format "%S (quote %S)" (car var) (cdr var)))
			     vars "\n      ")
			    "]\n" body ")")
		  body))))
  
    (car (org-babel-comint-with-output ("*inferior-lisp*" "ClojureScript")
	     (mapc
	      (lambda (line)
		(insert (org-babel-chomp line)) (comint-send-input nil t))
	      (list body (format "%S" "ClojureScript-eoe")))
	     (comint-send-input)))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:clojurescript (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-clojurescript-var-to-clojurescript (var)
  "Convert an elisp var into a string of clojurescript source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-clojurescript-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-clojurescript-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-clojurescript)
;;; ob-clojurescript.el ends here
