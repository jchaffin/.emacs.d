;;; init.el -- Emacs Initialization File
;;
;; Copyright (c) 2017 Jacob Chaffin
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, elisp, straight-el
;; Homepage: https://github.com/jchaffin/.emacs.d
;; Package-Requires: ((emacs "25"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;; This program is distributed in the hope that it will be useful,
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
(setq straight-repository-branch "develop")
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

;; The `straight-use-package' command fails when called interactively on the develop branch.
;; This is because gnu-elpa repository support isn't yet stable. For now, I'm bypassing
;; this issue by creating the directory if it does not exist when `straight-recipes-gnu-elpa-list'
;; is invoked.
(defadvice straight-recipes-gnu-elpa-list (around straight-recipe-gnu-elpa-list-around activate)
  (let* ((elpa-repo-dir (expand-file-name "straight/repos/elpa/" user-emacs-directory))
         (elpa-pkg-dir (expand-file-name "packages/" elpa-repo-dir)))
    (unless (file-exists-p elpa-pkg-dir)
      (if (file-exists-p elpa-repo-dir)
          (mkdir elpa-pkg-dir)))
    ad-do-it))

;; Clone use-package dependencies
(straight-use-package 'diminish)
(straight-use-package 'bind-key)
;; Now clone the `use-package' library
(straight-use-package 'use-package)
;; Use straight integration of `use-package'
(setq straight-use-package-version 'straight
      ;; And enable by default.
      straight-use-package-by-default t
      ;; pseudo packages
      straight-built-in-pseudo-packages '(emacs winner-mode
                                                artist-mode browse-url xwidget popup-el))

(setq use-package-always-defer t)

(straight-use-package 'git)

(eval-when-compile
  ;; String manipulated library bundled with Emacs >24.4.
  (require 'cl)
  (require 'dash)
  (require 'subr-x)
  (require 'git))

;; Add org-beautify-theme fork
(straight-use-package
  `(org-beautify-theme
    :host github
    :repo "jchaffin/org-beautify-theme"
    :local-repo-name org-beautify-theme))

;; https://github.com/raxod502/straight.el/issues/240
(straight-use-package
 `(auctex
   :host github
   :repo "jchaffin/auctex"
   :files (:defaults (:exclude "doc/*.texi"))))

;; Install org
;; https://github.com/raxod502/straight.el/tree/develop#installing-org-with-straightel
;; https://github.com/raxod502/radian/blob/master/radian-emacs/radian-org.el#L56-L92
(defun org-git-version ()
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))


(provide 'org-version)

(straight-use-package 'org-plus-contrib)

;; https://github.com/raxod502/straight.el/issues/72
;; https://github.com/raxod502/straight.el/issues/192
;; https://github.com/raxod502/straight.el/issues/168
(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c M-o" . org-store-link)
   (:map org-mode-map
         ("C-c M-t" . org-set-tags-command)
         ("C-c C-x h" . org-toggle-link-display)))

   :config
   (progn
     (when (eq system-type 'darwin)
       (setq org-directory (expand-file-name "~/Dropbox/org/")
             org-default-notes-file (concat org-directory "capture.org")))
     (setq org-insert-heading-respect-content t
           org-startup-indented t)))


(defvar literate-config-file "chaffin.org"
  "The *.org file containing the source code responsible for declaration and
configuration of third-party packages, as well as any settings and customizations
defined in this GNU Emacs distribution")

(defun load-literate (&optional user-config-file)
  (let ((target-file (or literate-config-file
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

(load-literate)






