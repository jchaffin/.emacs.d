;; -*- mode: emacs-lisp -*-
;;; init.el -- Emacs Initialization File
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; URL: https://github.com/jchaffin/.emacs.d
;;
;; This file is not part of GNU Emacs.


;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This file is loaded by Emacs at startup.

;;; Code

(if (eq system-type (or 'darwin 'gnu/linux))
    ;; On macOS, use straight.el as package manager
    ;; https://github.com/raxod502/straight.el
    ;; and tangle literate configuration file.
    ;; Otherwise, opt-in to features until this setup
    ;; has been tested on other operating systems.
    (progn

      (setq package-enable-at-startup nil)

      ;; Bootstrap straight.el
      (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
		(bootstrap-version 2))
	    (unless (file-exists-p bootstrap-file)
	      (with-current-buffer
		  (url-retrieve-synchronously
		   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		   'silent 'inhibit-cookies)
		(goto-char (point-max))
		(eval-print-last-sexp)))
	    (load bootstrap-file nil 'nomessage))


	(declare-function straight-use-package "straight.el")

	;; Load the default libraries
	(straight-use-package 'el-patch)
	(straight-use-package 'dash)
	(straight-use-package 'cl)

	(straight-use-package '(use-package
				 :host github
				 :repo "raxod502/use-package"
				 :upstream
				 (:host github
				  :repo "jwiegley/use-package")))

	(straight-use-package '(org
				  :host github
				  :repo "emacsmirror/org"
				  :local-repo-name org
				  :files ("lisp/*.el" "contrib/lisp/*.el")))

	(straight-use-package '(org-beautify-theme
				:host github
				:repo "jchaffin/org-beautify-theme"
				:upstream (:host github
						:repo "jonnay/org-beautify-theme")))

	;; [1] https://github.com/raxod502/straight.el/issues/168
	;; [2] https://github.com/raxod502/radian/blob/master/radian-emacs/radian-org.el
	;; See [2] For anything you'd ever want to know about whats going on here
	(use-package org
	    :commands (org-version)
	    :functions chaffin--org-git-version
	    :init
	    (progn
	      (require 'subr-x)
	      (defun chaffin--org-git-version ()
			(let ((default-directory (concat user-emacs-directory
						 "straight/repos/org/")))
		  (if (executable-find "git")
		      (with-temp-buffer
			(call-process "git" nil '(t nil) nil
				      "rev-parse" "--short" "HEAD")
			(if (> (buffer-size) 0)
			    (string-trim (buffer-string))
			  "revision unknown"))
		    "git not available")))
	      (defalias #'org-git-version #'chaffin--org-git-version)

	      (defun org-release () "N/A")
	      (provide 'org-version)

	      (with-eval-after-load 'org
						(defalias #'org-git-version #'chaffin--org-git-version)))

	    :config
	    (setq org-insert-heading-respect-content t
					  org-startup-indented t))


	(defun chaffin--org-init ()
	  ;; If tangling fails, setting these variables first
	  ;; ensures fontified, indented source code and no hassle
	  ;; of an unaliased yes-or-no for every subsequent extraction.
	  ;; Have a nice debugging session :smile:
	  (setq org-src-fontify-natively t
			 		org-confirm-babel-evaluate nil
					org-src-preserve-indentation t)
			   (org-babel-load-file (concat user-emacs-directory "chaffin.org")))

	(add-hook 'after-init-hook #'chaffin--org-init)
)

  ;; Otherwise
  (progn
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))))

;; init.el ends here.

