;;; ob-grapheasy.el --- org-babel integration for Graph::Easy            -*- lexical-binding: t; -*-

;; Filename: ob-grapheasy.el
;; Description: org-babel integration for Graph::Easy
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2020, Joe Bloggs, all rites reversed.
;; Created: 2020-02-17 00:36:57
;; Version: 0.1
;; Last-Updated: Fri Feb 17 00:36:15 2020
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/ob-grapheasy
;; Keywords: extensions
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((org "9.3.1"))


;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Joe Bloggs
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Features that might be required by this library:
;;
;;   org
;;

;;; Commentary:
;; 
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;; 
;; Org-Babel support for evaluating Graph::Easy graph descriptions, and printing the results
;; into an org file as ascii or unicode diagrams, or any other format supported by the graph-easy binary.
;;
;; For information on Graph::Easy see http://bloodgate.com/perl/graph/manual/index.html
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in Graph::Easy
;;
;; 2) the following extra header arguments are allowed: "infmt", "outfmt", "renderer" & "copy"
;;
;; 3) you must have the graph-easy perl script installed
;;
;; You can use the :var header argument to assign values to variables which can be referenced by prefixing
;; the variable name with a $ in the grapheasy code. For example.
;;
;; #+begin_src grapheasy :var a="foo" :var b="bar"
;; [$a]->[$b]
;; #+end_src
;;
;; which expands to:
;;
;; #+RESULTS:
;; ┌─────┐     ┌─────┐
;; │ foo │ ──> │ bar │
;; └─────┘     └─────┘
;;
;; The :copy header argument has the effect of skipping the evaluation step, but still performing variable substitution.
;; This enables you to reuse graphs from other code blocks, by referring to them in variable assignments, e.g:
;;
;; #+name: simple_network
;; #+begin_src grapheasy 
;; [Internet]<->[$modem]<->[$router]<->[$c1],[$c2]
;; #+end_src
;;
;; #+begin_src grapheasy :copy :var net=simple_network(modem="belkin",router="tomato",c1="foo",c2="bar")
;; My network:
;; $net
;; #+end_src
;;
;; #+RESULTS:
;; :results:
;; My network:
;; ┌──────────┐      ┌────────┐      ┌────────┐      ┌─────┐
;; │ Internet │ <──> │ belkin │ <──> │ tomato │ <──> │ bar │
;; └──────────┘      └────────┘      └────────┘      └─────┘
;;                                    ∧
;;                                    │
;;                                    │
;;                                    ∨
;;                                    ┌────────┐
;;                                    │  foo   │
;;                                    └────────┘
;; :end:



;;; Installation
;; 
;; To make sure you have the most up-to-date version of this library it is best to install 
;; using the emacs package system, with the appropriate repository added (e.g https://melpa.org/)
;; 
;; To install without using a package manager:
;; 
;;  - Put the library in a directory in the emacs load path, like ~/.emacs.d/
;;  - Customize the `org-babel-load-languages' so that it includes (grapheasy . t) in the list:
;;    M-x customize-variable org-babel-load-languages
;;  
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-babel-default-outfmt:grapheasy'
;;    Default output format for grapheasy source code blocks.
;;    default = "boxart"
;;  `org-babel-default-infmt:grapheasy'
;;    Default input format for grapheasy source code blocks.
;;    default = "txt"
;;  `org-babel-default-renderer:grapheasy'
;;    Default renderer to use for grapheasy source code blocks.
;;    default = "dot"

;;; Code:
(require 'ob)

(defcustom org-babel-default-outfmt:grapheasy "boxart"
  "Default output format for grapheasy source code blocks.
See the graph-easy manpage for info."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-default-infmt:grapheasy "txt"
  "Default input format for grapheasy source code blocks.
See the graph-easy manpage for info."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-default-renderer:grapheasy "dot"
  "Default renderer to use for grapheasy source code blocks.
See the graph-easy manpage for info."
  :type 'string
  :group 'org-babel)

(defvar org-babel-default-header-args:grapheasy
  '((:results . "output raw replace drawer") (:exports . "results"))
  "Default arguments to use when evaluating a grapheasy source block.")

(defun org-babel-expand-body:grapheasy (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:grapheasy (body params)
  "Execute a block of Graph::Easy code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY is the grapheasy soure code, and PARAMS is a list of header args & parameters."
  (let* ((fileparam (assq :file params))
	 (filep (or fileparam
		    (string-match
		     "file" (cdr (assq :results params)))))
	 (infmt (or (cdr (assq :infmt params))
		    org-babel-default-infmt:grapheasy))
	 (outfmt (or (cdr (assq :outfmt params))
		     org-babel-default-outfmt:grapheasy))
	 (renderer (or (cdr (assq :renderer params))
		       org-babel-default-renderer:grapheasy))
	 (outfile (if (or (member
			   outfmt ;; --output option is ignored if one of these formats is specified
			   '("bmp" "git" "hpgl" "jpg" "pcl" "pdf" "png" "ps" "ps2" "tga" "tif"))
			  (not (cdr fileparam)))
		      (concat "graph." outfmt)
		    (cdr fileparam)))
	 (cmd (or (cdr (assq :cmd params)) "graph-easy"))
	 (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
	 (coding-system-for-write 'utf-8)
	 (res (let ((body2 (org-babel-expand-body:grapheasy body params)))
		(if (assq :copy params)
		    body2
		  (org-babel-eval
		   (concat cmd " "
			   (when infmt (concat " --from=" infmt))
			   (concat " --as=" outfmt)
			   (when renderer (concat " --renderer=" renderer))
			   (when filep (concat " --output=" (org-babel-process-file-name outfile))))
		   body2)))))
    (if (not filep)
	res
      (if (assq :file params) ;; make sure correct file link is inserted
	  (setf (cdr (assq :file params)) outfile)
	(nconc params (list (cons :file outfile))))
      nil)))

(defun org-babel-prep-session:grapheasy (_session _params)
  "Return an error because Graph::Easy does not support sessions."
  (error "Graph::Easy does not support sessions"))

(provide 'ob-grapheasy)

;;; ob-grapheasy.el ends here
