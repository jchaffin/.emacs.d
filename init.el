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
;;
;;
;;; Code:

(setq debug-on-error t)
(unwind-protect
    (let ((straight-treat-as-init t))
      (setq debug-on-message "Invalid face attribute :family nil")
      (when (locate-library "gnutls")
        (require 'gnutls)
        ;; Prevent elpa from loading `package.el' in case loading fails.
        ;; Use LibreSSL certificates to bootstrap dependencies.
        ;; https://github.com/raxod502/straight.el/commit/7e77328b
        (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

      (setq straight-repository-branch "develop"
            ;; Use the macos lockfile
            straight-profiles '((dotemacs . "versions.el")
                                (nil . "default.el"))
            straight-current-profile 'dotemacs
            straight-recipes-gnu-elpa-use-mirror t)

      (if (and (executable-find "watchexec")
               (executable-find "python3"))
          (setq straight-check-for-modifications
                '(watch-files find-when-checking))
        (setq straight-check-for-modifications
              '(check-on-save find-when-checking)))


      ;; Bootstrap straight.el
      (let ((bootstrap-file
             (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

      ;; Clone use-package dependencies
      (straight-use-package 'bind-key)
      ;; Now clone the `use-package' library
      (straight-use-package 'use-package)
      ;; Enable the `:ensure-system-package' keyword
      (straight-use-package 'use-package-ensure-system-package)
      ;; Use  the `:blackout' to clean mode lighters
      (straight-use-package
       '(blackout :host github :repo "raxod502/blackout"))
      (require 'blackout)

      ;; lazy load by default
      (setq use-package-always-defer t)
      ;; Straight integration of `use-package'.
      ;; Allow built-in packages to be configured by `use-package'.
      (setq straight-use-package-version 'straight
            ;; And enable by default.
            straight-use-package-by-default t)

      (use-package no-littering
        :demand t
        :custom
        (no-littering-etc-directory (expand-file-name "etc" user-emacs-directory))
        (no-littering-var-directory (expand-file-name "var" user-emacs-directory)))

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
            (git-run "describe" "--match=release\*" "--abbrev=0" "HEAD")))))

      (provide 'org-version)


      (straight-use-package 'org-plus-contrib)

      ;; [[id:C2106106-C5F8-4B9B-815D-058678CB9242][Org Mode]]

      (use-package org
        :straight org-plus-contrib
        :custom
        (org-directory "~/Dropbox/org")
        (org-id-locations-file-name
         (expand-file-name "var/org/id-locations.el" user-emacs-directory))
        (org-src-fontify-natively t)
        (org-src-tab-acts-natively t)
        (org-src-preserve-indentation t)
        (org-src-persistent-message nil)
        (org-src-window-setup 'current-window)
        (org-hide-emphasis-markers t)
        (org-insert-heading-respect-content t)
        (org-ctrl-k-protect-subtree 'error)
        (org-blank-before-new-entry
         '((heading . auto)
           (plain-list-item . auto)))
        (org-catch-invisible-edits 'show)
        (org-yank-adjusted-subtrees t)
        (org-yank-folded-subtrees t)
        (org-modules '(org-bbdb
                       org-bibtex
                       org-crypt
                       org-elisp-symbol
                       org-eww
                       org-habit
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
                       org-registry
                       org-velocity))
        :bind*
        (("C-c L" . org-insert-link-global)
         ("C-c M-O" . org-open-at-point-global)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-s" . org-schedule)
         ("C-c b" . org-switchb)
         ("C-c M-o" . org-store-link)
         (:map org-mode-map
               ("C-c C-x h" . org-toggle-link-display)))

        :config
        (when (eq system-type 'darwin)
          (setq
                org-id-locations-file
                (expand-file-name "var/org/id-locations.el" user-emacs-directory)))

        (defun org-update-backends (val)
          "Update Emacs export backends while Emacs is running.
See `org-export-backends' variable."
          (interactive)
          (progn
            (setq org-export-registered-backends
                  (cl-remove-if-not
                   (lambda (backend)
                     (let ((name (org-export-backend-name backend)))
                       (or (memq name val)
                           (catch 'parentp
                             (dolist (b val)
                               (and (org-export-derived-backend-p b name)
                                    (throw 'parentp t)))))))
                   org-export-registered-backends))
            (let ((new-list (mapcar #'org-export-backend-name
                                    org-export-registered-backends)))
              (dolist (backend val)
                (cond
                 ((not (load (format "ox-%s" backend) t t))
                  (message
                   "Problems while trying to load export back-end
                   `%s'" backend))
                 ((not (memq backend new-list)) (push backend new-list))))
              (set-default 'org-export-backends new-list)))))



      ;; Literate
      (defcustom dotemacs-literate-config-file "dotemacs.org"
        "The *.org file containing the source code responsible for
      declaration and configuration of third-party packages, as well as
      any settings and customizations defined in this GNU Emacs
      distribution."
        :type 'string)

      (defcustom dotemacs-user-literate-init-file
        (expand-file-name dotemacs-literate-config-file user-emacs-directory)
        "The absolute path of `dotemacs-literate-config-file.'"
        :type 'string)

      (setq org-confirm-babel-evaluate nil)
      (org-babel-load-file
       (expand-file-name "dotemacs.org" user-emacs-directory)))

  (straight-finalize-transaction))

 ;;;; init.el ends here
