;;; init.el -- user initialization file for GNU Emacs
;;
;;; Copyright (c) 2019 Jacob Chaffin:
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, elisp, straight-el
;; Homepage: https://github.com/jchaffin/.emacs.d
;; Package-Requires: ((emacs "27"))
;;
;; This file is not part of GNU Emacs.

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

;;; Code:
(unwind-protect
    (let ((straight-treat-as-init t))
      (when (locate-library "gnutls")
        (require 'gnutls)
        ;; Prevent elpa from loading `package.el' in case loading fails.
        ;; Use LibreSSL certificates to bootstrap dependencies.
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))
;;;; straight
;;;;; configure straight
      (setq straight-repository-branch "develop"
            straight-profiles '((dotemacs . "versions.el")
                                (nil . "default.el"))
            straight-current-profile 'dotemacs)
      ;; Enable `straight-live-modifications-mode' if its dependencies are
      ;; found.
;;;;; straight live modifications:
      (if (and (executable-find "watchexec")
               (executable-find "python3"))
          (setq straight-check-for-modifications
                '(watch-files find-when-checking))
        (setq straight-check-for-modifications
              '(check-on-save find-when-checking)))
;;;;; Bootstrap straight.el:
      (let ((bootstrap-file
             (expand-file-name
              "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5)
            (domain "https://raw.githubusercontent.com")
            (repo "raxod502/straight.el")
            (branch straight-repository-branch)
            (remote-file "install.el"))
        ;; TODO Add error handling if straight fails to install or load.
        ;; If anything goes wrong here it won't throw until the unwind form
        ;; at the end of this file when `straight-finalize-transaction'
        ;; isn't defined.
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               (mapconcat #'identity (list domain repo branch remote-file) "/")
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))
;;;;; Configure use-package:
      ;; Enable the `:bind-key' keyword
      (straight-use-package 'bind-key)
      ;; Now clone the `use-package' library
      (straight-use-package 'use-package)
      ;; Enable the `:ensure-system-package' keyword
      (straight-use-package 'use-package-ensure-system-package)
      ;; blackout
      ;; Use `blackout' to clean mode lighters, essentially a drop in
      ;; replacement for ':diminish'
      (straight-use-package
       '(blackout :host github :repo "raxod502/blackout"))
      (require 'blackout)
      ;; lazy load by default
      (setq use-package-always-defer t)
      ;; Enable the newer version of `use-package'.
      (setq straight-use-package-version 'straight
            straight-use-package-by-default t)
      ;; reduce the clutter in `user-emacs-directory'
      (use-package no-littering
        :demand t
        :commands (no-littering-expand-etc-file-name)
        :custom
        (no-littering-etc-directory (expand-file-name "etc" user-emacs-directory))
        (no-littering-var-directory (expand-file-name "var" user-emacs-directory))
        :init
        (require 'recentf)
        ;; exclude from recentf
        (add-to-list 'recentf-exclude no-littering-var-directory)
        (add-to-list 'recentf-exclude no-littering-etc-directory)
        ;; store auto save files in the var directory.
        :config
        (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

      (customize-set-variable 'load-prefer-newer t) ;; load newer bytecode
      (use-package auto-compile
        :demand t
        :init
        (auto-compile-on-load-mode))
      ;; `el-patch' is like advice, but with state awareness and validation.
      (straight-use-package 'el-patch)
      (require 'subr-x)
      (straight-use-package 'git)

;;;; Org Mode:
      ;; Make straight.el and org-mode play nice.
      ;; The Org mode build system autogenerates an "org-versions.el" file
      ;; which in turn is used for loading org libraries and definitions.
      ;; If the built-in Org mode is loaded before straight version then
      ;; Org mode will not function correctly.
      ;;
      ;; These two functions, `org-git-version' and `org-release', are generated by
      ;; `org-make-org-version' at build time and used by `org-version' to
      ;; load `org' during initialization. These functions need to be defined
      ;; before the loading the straight version of Org, which prevents
      ;; Emacs from trying to load the built-in library.
      ;; See [[https://github.com/raxod502/straight.el/issues/211][straight.el/issues/#211]]
;;;;; org-release:

      (defun org-release ()
        "The release version of org-mode.
      Inserted by installing org-mode or when a release is made."
        (require 'git)
        (let ((git-repo (expand-file-name "straight/repos/org/" user-emacs-directory)))
          (string-trim
           (string-remove-prefix
            "release_"
            (git-run "describe" "--match=release\*" "--abbrev=0" "HEAD")))))
;;;;; org-git-version:

      (defun org-git-version ()
        "The Git version of Org Mode.
Inserted by installed Org or when a release is made."
        (require 'git)
        (let ((git-repo (expand-file-name "straight/repos/org/" user-emacs-directory)))
          (string-trim
           (git-run "describe" "--match=release\*" "--abbrev=6" "HEAD"))))
      (provide 'org-version)
;;;;; install org mode:
      (straight-use-package 'org-plus-contrib)

      (use-package org
        :straight org-plus-contrib
        :custom
        ;; Org Files
        (org-directory (file-truename "~/Dropbox/org"))
        ;; setup archive directory in current  folder
        (org-archive-location "archive/%s_archive::")
        ;; Source Blocks
	      (org-confirm-babel-evaluate nil)
        (org-src-fontify-natively t)
        (org-src-tab-acts-natively t)
        (org-src-preserve-indentation t)
        (org-src-persistent-message nil)
        (org-src-window-setup 'current-window)
        (org-ctrl-k-protect-subtree 'error)
        (org-catch-invisible-edits 'smart)
        ;; Structure
        (org-hide-emphasis-markers t)
        (org-use-sub-superscripts '{})
        (org-blank-before-new-entry
         '((heading . auto)
           (plain-list-item . auto)))
        (org-list-allow-alphabetical t)
        (org-yank-adjusted-subtrees t)
        (org-yank-folded-subtrees t)
        (org-use-speed-commands t)
        (org-display-internal-link-with-indirect-buffer t)
        (org-modules '(org-bbdb
                       org-bibtex
                       org-crypt
                       org-elisp-symbol
                       org-eww
v                       org-habit
                       org-id
                       org-info
                       org-inlinetask
                       org-protocol
                       org-tempo
                       org-eshell
                       org-annotate-file
                       org-bookmark
                       org-checklist
                       org-collector
                       org-mac-iCal
                       org-mac-link
                       org-man
                       org-velocity))
        :bind
        (("C-c L" . org-insert-link-global)
         ("C-c M-O" . org-open-at-point-global)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("C-c M-o" . org-store-link)
         (:map org-mode-map
               ("C-c C-x h" . org-toggle-link-display)
               ("C-c C-s" . org-schedule))))

;;;; Literate:
      (defgroup dotemacs nil
        "Customization group for the `dotemacs' Emacs configuration."
        :group 'applications
        :prefix "dotemacs-")

      (defcustom dotemacs-literate-config-file
        (expand-file-name "dotemacs.org" user-emacs-directory)
        "The *.org file containing the source code responsible for
      declaration and configuration of third-party packages, as well as
      any settings and customizations defined in this GNU Emacs
      distribution."
        :type 'file
        :group 'dotemacs)

      (defvar literate-debug-blocks
        '("core/read-only"
          "core/ox"
          "core/ivy"
          "core/counsel"
          "core/paredit"
          "core/elisp"
          "core/rainbow-delimiters"
          "core/swiper")
        "Named source blocks to tangle when `use-literate-p' is enabled. ")

      ;; Extract source code and load the config
      (if use-literate-p
          (if (file-exists-p dotemacs-literate-config-file)
              (org-babel-load-file dotemacs-literate-config-file)
            (error "File does not exist %s" dotemacs-literate-config-file))
        (mapcar #'literate-tangle-src-block literate-debug-blocks))
      (setq initial-buffer-choice dotemacs-literate-config-file))
  (straight-finalize-transaction))
;;; init.el ends here
