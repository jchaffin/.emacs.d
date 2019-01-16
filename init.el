;;; init.el -- Emacs Initialization File
;;
;; Copyright (c) 2018 Jacob Chaffin
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, elisp, straight-el
;; Homepage: https://github.com/jchaffin/.emacs.d
;; Package-Requires: ((emacs "27"))
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
;; [3] https://github.com/jchaffin/.emacs.d/blob/master/halidom.org#literate-programming
;; [4] http://orgmode.org/worg/org-contrib/babel/

;;; Code:
;; Straight
(unwind-protect
    (let ((straight-treat-as-init t))
      (when (locate-library "gnutls")
        (require 'gnutls)
        ;; Prevent elpa from loading `package.el' in case loading fails.
        ;; Use LibreSSL certificates to bootstrap dependencies.
        ;; https://github.com/raxod502/straight.el/commit/7e77328b
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

      (setq straight-repository-branch "develop"
            ;; Use the macos lockfile
            straight-profiles '((halidom . "versions.el")
                                (nil . "default.el"))
            straight-current-profile 'halidom
            straight-recipes-gnu-elpa-use-mirror t)

      (if (and (executable-find "watchexec")
               (executable-find "python3"))
          (setq straight-check-for-modifications
                '(watch-files find-when-checking))
        (setq straight-check-for-modifications
              '(check-on-save find-when-checking)))



      ;; Bootstrap straight.el
      (let ((bootstrap-file
             (concat user-emacs-directory "straight/bootstrap.el"))
            (bootstrap-version 2)
            (domain "https://raw.githubusercontent.com")
            (repo "raxod502/straight.el")
            (branch straight-repository-branch)
            (remote-file "install.el"))

        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               (mapconcat #'identity
                          (list domain repo branch remote-file) "/")
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))

      ;; Clone use-package dependencies
      (straight-use-package 'bind-key)
      ;; Now clone the `use-package' library
      (straight-use-package 'use-package)
      ;; Enable the `:ensure-system-package' keyword
      (straight-use-package 'use-package-ensure-system-package)
      ;; Use  the `:blackout' to clean mode lighters
      (straight-use-package '(blackout
                              :host github
                              :repo "raxod502/blackout"))
      (require 'blackout)

      ;; lazy load by default
      (setq use-package-always-defer t)
      ;; Straight integration of `use-package'.
      ;; Allow built-in packages to be configured by `use-package'.
      (setq straight-use-package-version 'straight
            ;; And enable by default.
            straight-use-package-by-default t)
      ;; https://github.com/raxod502/el-patch#lazy-loading-packages
      (straight-use-package 'el-patch)

      (straight-use-package 'git)

      ;; Install org
      ;; See the [[https://github.com/raxod502/straight.el/tree/develop#installing-org-with-straightel][Known Issue FAQ]]
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
      ;; [[id:C2106106-C5F8-4B9B-815D-058678CB9242][Org Mode]]

      (use-package org
        :init
        (defun halidom/resolve-org-ivy-conflict ()
          ;; Conflicts with `ivy-resume'
          (define-key org-mode-map (kbd "C-c C-r") nil))

        :custom
        (org-startup-indented t)
        (org-src-fontify-natively t)
        (org-src-tab-acts-natively t)
        (org-src-preserve-indentation t)
        (org-src-window-setup 'other-window)
        (org-confirm-babel-evaluate nil)
        (org-edit-src-persistent-message nil)
        (org-catch-invisible-edits t)
        (org-babel-uppercase-example-markers t)
        (org-hide-block-startup t)
        (org-hide-leading-stars t)
        (org-hide-emphasis-markers t)
        :bind
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture)
        ("C-c C-s" . org-schedule)
        ("C-c M-o" . org-store-link)
        ("C-c C-l" . org-insert-link)
        ("C-c b" . org-switchb)

        (:map org-mode-map
              ("C-c M-t"   . org-set-tags-command)
              ("C-c C-x h" . org-toggle-link-display))

        :config
        (when (eq system-type 'darwin)
          (setq org-directory (file-truename "~/Dropbox/org/")
                org-id-locations-file
                (expand-file-name
                 "var/org/id-locations.el" user-emacs-directory))))

      (use-package no-littering
	      :init
	      (setq no-littering-etc-directory
	            (expand-file-name "etc/" user-emacs-directory))
	      (setq no-littering-var-directory
	            (expand-file-name "var/" user-emacs-directory))
	      (require 'no-littering))


      (cl-letf (((symbol-function 'var)
                 (symbol-function #'no-littering-expand-var-file-name)))
        (with-no-warnings
          (setq auto-save-file-name-transforms
	              `((".*" ,(var "auto-save/") t)))
          (setq srecode-map-save-file (var "srecode-map.el"))))

      (require 'recentf)
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory)

      (use-package org-dotemacs
	      :custom
	      (org-dotemacs-default-file "~/.emacs.d/dotemacs.org")
	      :init
	      (require 'org-dotemacs))


      (org-dotemacs-load-file "compile" org-dotemacs-default-file
        (expand-file-name "dotemacs.el" user-emacs-directory)))
  (straight-finalize-transaction))

 ;;;; init.el ends here
