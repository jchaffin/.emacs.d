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
       (straight-use-package 'diminish)
       (straight-use-package 'bind-key)
       ;; Now clone the `use-package' library
       (straight-use-package 'use-package)
       ;; Enable the `:ensure-system-package' keyword
       (straight-use-package 'use-package-ensure-system-package)
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
         (org-ellipsis "  ") ;; folding symbol
         (org-insert-heading-respect-content t)
         (org-pretty-entities t)
         (org-hide-emphasis-markers t)
         (org-fontify-whole-heading-line t)
         (org-fontify-done-headline t)
         (org-fontify-quote-and-verse-blocks t)
         (org-src-fontify-natively t)
         (org-confirm-babel-evaluate nil)


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
	          (setq org-directory (expand-file-name "~/Dropbox/org/")
		              org-default-notes-file (expand-file-name
					                                "capture.org"
					                                org-directory)
		              org-id-locations-file
		              (expand-file-name "var/org/id-locations.el"
				                            user-emacs-directory)))
          
          :hook
          (org-mode . halidom/resolve-org-ivy-conflict))

       ;; Literate
       (defcustom halidom-literate-config-file "halidom.org"
         "The *.org file containing the source code responsible for
      declaration and configuration of third-party packages, as well as
      any settings and customizations defined in this GNU Emacs
      distribution."
         :type 'string)

       (defcustom halidom-user-literate-init-file
         (expand-file-name halidom-literate-config-file user-emacs-directory)
         "The absolute path of `halidom-literate-config-file.'"
         :type 'string)


       (defun load-literate (&optional user-config-file)
         "If USER-CONFIG-FILE is passed as an argument, then tangle.
    Else use the value of `halidom-literate-config-file'."
         (let ((target-file (or user-config-file halidom-literate-config-file))
               (target-dir (or user-emacs-directory default-directory)))

           (if target-file
               (org-babel-load-file
                (expand-file-name target-file target-dir))
             (error "%s not found, cannot tangle." target-file))))


       ;; Debug
       (defvar halidom-literate-debug-blocks
         '("halidom/functions"
           "readview-fc"
           "halidom/macros"
           "org-ui-fill"
           "ivy-spec"
           "counsel-spec"
           "mb-swiper"
           "paredit-spec")
         )


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
         (let ((buf (find-file-noselect halidom-user-literate-init-file)))
           (with-current-buffer halidom-literate-config-file
             (org-element-map (org-element-parse-buffer) 'src-block
               (lambda (block)
                 (if (string= name (org-element-property :name block))
                     (let ((code (org-element-property :value block))
                           (params (org-element-property :parameters block))
                           (noweb-p (literate-src-block-noweb-p
                                     (org-element-property :parameters block))))

                       (if noweb-p
                           (mapcar #'literate-tangle-src-block (sanitize-no-web-block code))
                         (with-temp-buffer
			     (message "%s" code)
                             (insert code)
			     (eval-buffer))))))))
           (kill-buffer buf)))

       (defun literate-debug-enabled ()
         "Tangle only the source blocks with a name property matching an element in 
    `halidom-literate-debug-blocks'. 

    This is useful for providing a set of defaults for debugging purposes."

         (interactive)
         (mapcar #'literate-tangle-src-block halidom-literate-debug-blocks))

       ;; Initialization
       (when (< emacs-major-version 27)
	   (setq use-literate-p t))

       (if use-literate-p
           (load-literate halidom-literate-config-file)
         (literate-debug-enabled)
         (paredit-mode 1)))

  (straight-finalize-transaction))


 ;;;; init.el ends here
