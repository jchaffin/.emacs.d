;;; init.el -- Emacs Initialization File
;;
;; Copyright (c) 2012-2017 Jacob Chaffin
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, elisp, straight-el
;; Homepage: https://github.com/jchaffin/.emacs.d
;; Package-Requires: ((emacs "25")
;; 
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;; ;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This Emacs configuration uses straight.el as a drop-in replacement for
;; package.el. A thorough comparative analysis of the two libraries as
;; well as other popular tools for package management can be found in the
;; README of the straight.el repository [1].
;;
;; This initialization process uses the tangle functionality of org-babel to
;; extract the source code configuring package blocks
;; For Donald Knuths seminal text on Literate Programimng, see [2]
;; For my synopis on the subject, see [3]
;; For writing code in org mode using babel, see [4]
;;
;; [1] https://github.com/raxod502/straight.el/blob/master/README.md
;; [2] http://www.literateprogramming.com/knuthweb.pdf
;; [3] https://github.com/jchaffin/.emacs.d/blob/master/chaffin.org#literate-programming
;; [4] http://orgmode.org/worg/org-contrib/babel/

;;; Code:

;; Bootstrap straight.el from develop branch
;; https://github.com/raxod502/straight.el#getting-started
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


;; Clone use-package dependencies
(straight-use-package 'diminish)
(straight-use-package 'bind-key)
;; Now clone the `use-package' librariy
(straight-use-package 'use-package)
;; Use straight integration of `use-package'
(setq straight-use-package-version 'straight)
 ;; And enable by default.
(setq straight-use-package-by-default t)
;; Ensure that the `use-package' library and dependencies
;; are avaible at compile-time.
(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))
;; Common Lisp extensions
;; Use separate version then the one bundled
;; with Emacs
(straight-use-package 'cl)
;; Dash provides a modern list api for Emacs.
;; At the time of this writing, it's the most downloaded
;; package on melpa. Several of the most popular Emacs
;; libraries use `dash' and/or `dash-functional' as
;; required dependencies.
(straight-use-package 'dash)
(straight-use-package 'dash-functional)
;; And ensure these libraries are available at compile-time.
(eval-when-compile
  (require 'cl)
  (require 'dash)
  ;; String manipulated library bundled with Emacs >24.4.
  (require 'subr-x))

;; Install org
(straight-use-package
  `(org
    :host github
    :repo "emacsmirror/org"
    :local-repo-name org
    :files ("lisp/*.el" "contrib/lisp/*.el")))

;; We're using org to extract the source code from
;; our configuration file. The Emacs loading process
;; can be tricky when using external lisp libraries which are 
;; bundled in the Emacs distribution.
;; [5] https://github.com/raxod502/straight.el/issues/72
;; [6] https://github.com/raxod502/straight.el/issues/192
;; [7] https://github.com/raxod502/straight.el/issues/168
;; [8] https://github.com/raxod502/radian/blob/ee92ea6cb0473bf7d20c6d381753011312ef4a52/radian-emacs/radian-org.el#L46-L112

(use-package org
  :bind
  (("C-c a" . org-agenda))
  :init
  ;; The follwing hacks are designed to trick the version checking
  ;; done in the loading process of org into recognizing the mirror
  ;; as the correct org package [8]. Unfortunately, `org-version' still
  ;; throws a warning upon initial package installation, but it appears
  ;; the hack corrects the version checking in all subsequent startups.
  ;; See [6] for a discussion on issue at hand, and [8] for what how
  ;; this implementation addresses the problem. 
  (progn
    (defun chaffin--org-git-version ()
      (let ((default-directory
        (concat
          (or user-emacs-directory
             (expand-file-name "~/.emacs.d/"))
          "straight/repos/org")))
        (if (executable-find "git")
            (with-temp-buffer
              (call-process "git" nil '(t nil) nil
                            "rev-parse" "--short" "HEAD")
              (if (> (buffer-size) 0)
                  (string-trim (buffer-string))
                "revision unknown"))
          "Git not installed!")))
    ;; Override triggering the autoloads from the bundled org package.
    ;; Again, see [8]; raxod502 can offer better insight here than I can.
    (defalias #'org-git-version #'chaffin--org-git-version)
    (defun org-release () "9.1.3")
    (provide 'org-version))
  
  :config
  (setq org-insert-heading-respect-content t
        org-startup-indented t))

(defvar user-emacs-literate-config-file nil
  "The *.org file containing the source code responsible for declaration and 
configuration of third-party packages, as well as any settings and customizations
defined in this GNU Emacs distribution")

(defun user-emacs-load-config (&optional user-config-file)
  (let ((target-file (or user-emacs-literate-config-file
                         user-config-file))
        (target-dir (or user-emacs-directory
                        default-directory)))
        (setq org-src-fontify-natively t
          org-confirm-babel-evaluate nil
          org-src-preserve-indentation t)
    (if target-file
        (org-babel-load-file
         (expand-file-name target-file target-dir))
      (message "No configuration file set, not extracting source code."))))

(setq user-emacs-literate-config-file "chaffin.org")
(user-emacs-load-config)





