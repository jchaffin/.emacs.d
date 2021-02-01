;;; init.el -- User initialization file for GNU Emacs
;;

;; Copyright (c) 2019 Jacob Chaffin:
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
    (let ((straight-treat-as-init t))
      (when (locate-library "gnutls")
        (require 'gnutls)
        ;; Prevent elpa from loading `package.el' in case loading fails.
        ;; Use LibreSSL certificates to bootstrap dependencies.
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))
;;; straight
;;;; Variables
      (setq straight-repository-branch "develop"
            straight-profiles '((dotemacs . "versions.el")
                                (nil . "default.el"))
            straight-current-profile 'dotemacs)
      ;; Enable `straight-live-modifications-mode' if its dependencies are
      ;; found.
;;;; straight live modifications:
      (if (and (executable-find "watchexec")
               (executable-find "python3"))
          (setq straight-check-for-modifications
                '(watch-files find-when-checking))
        (setq straight-check-for-modifications
              '(check-on-save find-when-checking)))
;;;; Bootstrap straight.el:
      (let ((bootstrap-file
             (expand-file-name
              "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5)
            (domain "https://raw.githubusercontent.com")
            (repo "raxod502/straight.el")
            (branch straight-repository-branch)
            (remote-file "install.el"))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               (mapconcat #'identity (list domain repo branch remote-file) "/")
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))
;;;; use package
      ;; Enable the `:bind-key' keyword
      (straight-use-package 'bind-key)
      ;; Now clone the `use-package' library
      (straight-use-package 'use-package)
      ;; Enable the `:ensure-system-package' keyword
      (straight-use-package 'use-package-ensure-system-package)
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
;;;; no littering
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
;;;; auto compile
      (use-package auto-compile
        :demand t
        :init
        (auto-compile-on-load-mode))
      ;; `el-patch' is like advice, but with state awareness and validation.
      (straight-use-package 'el-patch)
      (require 'subr-x)
      (straight-use-package 'git)

;;; Org
;;;;; install org mode:
      (straight-use-package 'org-plus-contrib)
;;;;; Org configuration
      (use-package org
        :straight org-plus-contrib
;;;;;; customizations
        :custom
;;;;;;; Files
        (org-directory (file-truename "~/Dropbox/org"))
        ;; setup archive directory in current folder
        (org-archive-location "archive/%s_archive::")
;;;;;;; Org source
	      (org-confirm-babel-evaluate nil)
        (org-src-fontify-natively t)
        (org-src-preserve-indentation t)
        (org-src-persistent-message nil)
        (org-src-window-setup 'current-window)
        (org-ctrl-k-protect-subtree 'error)
        (org-startup-indented t)
        (org-catch-invisible-edits 'smart)
;;;;;;; Structure and Appearance
        (org-display-remote-inline-images 'cache)
        (org-insert-heading-respect-content t)
        (org-ellipsis "")
        (org-list-allow-alphabetical t)
        (org-hide-emphasis-markers t)
        (org-hidden-keywords '(author title date))
        (org-pretty-entities t)
        (org-use-sub-superscripts '{})
        (org-use-speed-commands t)
        (org-yank-folded-subtrees t)
        (org-yank-adjusted-subtrees t)
        (org-blank-before-new-entry
         '((heading . auto)
           (plain-list-item . auto)))
;;;;;;; org libraries
        (org-modules '(ol-bookmark
                       ol-man
                       ol-elisp-symbol
                       org-bbdb
                       org-bibtex
                       org-crypt
                       org-eww
                       org-habit
                       org-id
                       org-info
                       org-inlinetask
                       org-protocol
                       org-tempo
                       org-eshell
                       org-annotate-file
                       org-checklist
                       org-collector
                       org-mac-iCal
                       org-mac-link
                       org-velocity))
;;;;;; org keybindings
        :bind
        (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("C-<tab>" . org-global-cycle)
         (:map org-mode-map
               ("C-c C-x h" . org-toggle-link-display)
               ("C-c C-s" . org-schedule))))
;;; Literate
;;;; Custom variables
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
          "core/rainbow-delimiters"
          "core/lisps"
          "core/elisp"
          "core/swiper"
	  "core/helpful")

        "Named source blocks to tangle when `use-literate-p' is enabled. ")
;;;; tangle and load
      ;; Extract source code and load the config
  (defun literate-src-parameter-string->alist (parameters)
         "Convert src block parameter string into a plist."
         (let ((params-list (split-string parameters " " t)))
           (cl-loop for x in params-list
              for i from 1 to (length params-list)
              if (cl-oddp i)
              collect (intern x) into odds
              else
              collect x into evens
              end
              finally (return (seq-mapn #'cons odds evens)))))


       (defun literate-src-block-noweb-p (parameters)
         (let ((params-alist (literate-src-parameter-string->alist parameters)))
           (string= "yes" (cdr (assoc :noweb params-alist)))))


       (defun sanitize-no-web-block (code)
         (let ((sx (split-string code "\n" t)))
           (cl-flet ((func (s)
                       (replace-regexp-in-string "<<\\(.*?\\)>>" "\\1" s)))
             (mapcar #'func sx))))

       (defun literate-tangle-src-block (name)
         (let ((buf (find-file-noselect dotemacs-literate-config-file)))
           (with-current-buffer (get-file-buffer dotemacs-literate-config-file)
             (unwind-protect
                 (org-element-map (org-element-parse-buffer) 'src-block
                (lambda (block)
                  (if (string= name (org-element-property :name block))
                      (let ((code (org-element-property :value block)))
                          (with-temp-buffer
                            (insert code)
                            (eval-buffer)))))))))))

(if use-literate-p
    (if (file-exists-p dotemacs-literate-config-file)
        (org-babel-load-file dotemacs-literate-config-file)
      (error "File does not exist %s" dotemacs-literate-config-file))
  (mapcar #'literate-tangle-src-block literate-debug-blocks)
  (setq initial-buffer-choice dotemacs-literate-config-file))
;;; init.el ends here
