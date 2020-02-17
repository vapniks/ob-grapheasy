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
;; 2) the following extra header arguments are allowed: "outfile", "infmt", "outfmt" & "renderer" 
;;
;; 3) you must have the graph-easy perl script installed

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
;;  `org-babel-default-outfmt'
;;    Default output format for grapheasy source code blocks.
;;    default = "boxart"

;;; Code:
(require 'ob)

(defcustom org-babel-default-outfmt "boxart"
  "Default output format for grapheasy source code blocks.
Can be one of; \"ascii\", \"boxart\", \"html\", \"svg\", \"graphviz\"/\"dot\",
\"txt\", \"vcg\", \"gdl\", or \"graphml\".
See the graph-easy manpage for more info.")

(defvar org-babel-default-header-args:grapheasy
  '((:results . "output raw") (:exports . "results"))
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
  (let* ((outfile (cdr (assq :outfile params)))
	 (infmt (cdr (assq :infmt params)))
	 (renderer (cdr (assq :renderer params)))
	 (outfmt (cdr (assq :outfmt params)))
	 (cmd (or (cdr (assq :cmd params)) "graph-easy"))
	 (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
	 (coding-system-for-write 'utf-8)
	 (res (org-babel-eval
	       (concat cmd " "
		       (when infmt (concat " --from=" infmt))
		       (concat " --as=" (or outfmt org-babel-default-outfmt))
		       (when renderer (concat " --renderer=" renderer))
		       (when outfile (concat " --output=" (org-babel-process-file-name outfile))))
	       (org-babel-expand-body:grapheasy body params))))
    (unless outfile res)))

(defun org-babel-prep-session:grapheasy (_session _params)
  "Return an error because Graph::Easy does not support sessions."
  (error "Graph::Easy does not support sessions"))

(provide 'ob-grapheasy)

;;; ob-grapheasy.el ends here