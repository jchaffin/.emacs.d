;;; halidom.el -- Emacs Configuration File
;;
;; Copyright (c) 2018 Jacob Chaffin
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, dotemacs, org-mode, literate
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
;; Commentary:
;; This file was tangled using `org-babel'. See [[file:chaffin.org::#tangle-file][chaffin.org]] for the source.

;;; Code:

(defun goto-repo (&optional build-dir)
  "Go to a straight repository directory. If BUILD-DIR, then go to
the build directory for that repository instead."
  (interactive "P")
  (let* ((dir (-> user-emacs-directory
                  (f-join "straight"
                          (if build-dir "build" "repos"))))
         (msg (format "(%s) Goto recipe: " (upcase-initials (f-base dir)))))
    (ivy-read
     msg
     (directory-files dir)
     :action (lambda (package)
               (dired (f-join dir package))))))

  ;; Add to goto-* keymap
  (define-key goto-map "r" #'goto-repo)

(defun straight--get-remote-url (pkg)
  (let ((recipe (cdr (straight-recipes-retrieve pkg))))
    (destructuring-bind (repo host)
        `(,(plist-get recipe :repo)
          ,(plist-get recipe :host))
      (if (eq host 'github)
          (concat "https://github.com/" repo)
        (message "%s is not a GitHub repository :(" pkg)))))


(defun straight-browse-on-github (&optional package)
  "Open a straight recipe on GitHub in the `browse-url-default-browser.'"
  (interactive "P")
  (let* ((pkg (if (interactive-p)
                  (completing-read
                   "Which recipe? "
                   (straight-recipes-list straight-recipe-repositories)
                   nil 'require-match)))
         (url (straight--get-remote-url (intern pkg))))
    (browse-url url)))

(setq user-full-name "Jacob Chaffin"
      user-mail-address "jchaffin@ucla.edu")

(setq user-emacs-directory "~/.emacs.d/")

;; Customization Group
(defgroup halidom nil
  "Customization group for the `halidom' Emacs configuration."
  :group 'applications
  :prefix "halidom-")

(defcustom halidom-prefix "\M-m"
  "The prefix map leader key.")

(use-package dash
  :config
  (eval-after-load 'dash
    '(dash-enable-font-lock)))

(use-package cl-lib)

(use-package cl-lib-highlight
  :demand t
  :after cl-lib)

(use-package ov
  :straight t)

(use-package f)

(use-package s)

(use-package el-patch
    :init
  (defun el-patch-remove-feature ()
    (interactive)
    (let ((feature (completing-read "Feature: " el-patch-pre-validate-hook))
          (patch (call-interactively #'el-patch-unpatch)))
      (remove-hook 'el-patch-pre-validate-hook (intern feature))
      (remhash patch el-patch--patches))))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows))

;; Begin Macros
;; If not
(defmacro if-not (condition then-form &rest rest-forms)
  (declare (indent 2))
  `(progn
     (if (not ,condition)
	 ,then-form
       ,@rest-forms)))
;; with major mode
(defmacro with-major-mode (mode &rest body)
  "If the current major-mode is MODE, then execute BODY."
  (declare (indent defun))
  `(when (equal major-mode ',mode)
     ,@body))
;; if major mode
(defmacro if-major-mode (mode then-form &rest rest-forms)
  "If MODE, then execute THEN-FORM, else execute REST-FORMS."
  (declare (indent defun))
  `(progn
     (if (equal major-mode ',mode)
	       ,then-form
       ,@rest-forms)))
;; End Macros

;; Add to Hooks
(defun add-to-hooks (fun hooks)
  "Add function FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

;; Join Strings

(defun join (seq sep)
  "Concatenate SEQ with SEP as combinator."
  (mapconcat 'identity seq sep))

;; SSL check

(defun ssl-p ()
  "Determine whether https protocol is supported."
  (and (not (memq system-type '(windows-nt ms-dos)))
       (gnutls-available-p)))

;; Minor Modes
(defun list-enabled-minor-modes (&optional buf)
  "The minor modes enabled in the current buffer."
  (let ((auto-save-mode nil)
        (buf (or buf (current-buffer))))
    (cl-loop for mode being the element of minor-mode-list
             when (boundp mode)
             when (symbol-value mode)
             collect mode)))


(defvar minor-modes-enabled-list (list-enabled-minor-modes (current-buffer))
  "The list of enabled minor modes")

(defun minor-mode-enabled-p (mode)
  (member mode (list-enabled-minor-modes (current-buffer))))
;; Unadvise

(defun unadvise (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))



(defun scratch (&optional new)
  "Switch to scratch buffer. If optional prefix NEW,
then create a new buffer. Else reuse the existing scratch buffer,
generating a new one if the initial scratch buffer has been killed."
  (interactive "P")
  (unless (or new (not (seq-contains (buffer-list) (get-buffer "*scratch*"))))
    (with-current-buffer (generate-new-buffer "*scratch*")
      (emacs-lisp-mode)))
  (switch-to-buffer-other-window "*scratch*"))


;; Window Count
(defun window-count ()
  "Count number of windows in the current frame."
  (interactive)
  (length (window-list)))
;; Window Count Unique
(defun window-count-unique ()
  "Count number of unique windows in the current frame"
  (interactive)
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list)))))
;; Window Buffer List
(defun window-buffer-list ()
  "Get list of buffers in an open window."
  (let ((windows))
    (dolist (frame (frame-list) windows)
      (with-selected-frame frame
      (setq windows (append (window-list) windows))))
        (map 'seq-uniq (lambda (w) (window-buffer w)) windows)))
;; WIndow Buffer List Modes

;; Unix Basename
(defun basename (pathname)
  "Return the filename or directory portion of PATHNAME"
  (if (or (file-directory-p pathname)
          (string-match "/$" pathname))
      (let ((dirname (directory-file-name pathname)))
        (file-name-nondirectory dirname))
    (file-name-nondirectory pathname)))
;; Copy File Path
(defun copy-file-path (func)

  "Copies the file path and applies the result as an argument to
function FUNC. To copy the file path to the kill-ring, use the
 interactive function `copy-file-path-as-kill'."

  (destructuring-bind (file dir)
      (cond ((eq major-mode 'dired-mode)
             (list (substring-no-properties (thing-at-point 'symbol))
                   dired-directory))
            ((stringp buffer-file-name)
             (mapcar (lambda (f) (funcall f buffer-file-name))
                     '(file-name-nondirectory file-name-directory)))
            (t  (list (buffer-name (current-buffer)) default-directory)))
    (funcall func (expand-file-name file dir))))

(defun copy-file-path-as-kill ()
  "Copies the file path of the current dired directory or file buffer to the kill-ring."
  (interactive)
  (let ((func (lambda (s)
		(progn
		  (kill-new s)
		  (message "%s" s)))))
    (copy-file-path func)))

;; Directory Files no wildcards
(defun directory-files-no-wildcards (directory &optional full nosort)
   "List directory contents without wildcards"
   (cddr (directory-files directory full nil nosort)))
;; Read File
;; As String
(defun read-file-contents (file)
  "Return contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))
;; As list
(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))
;; Resolve Path

(defun resolve-path (&rest paths)
  "Concatenate path segments."
  (let ((paths- (mapcar #'directory-file-name paths)))
    (mapconcat 'identity paths- "/")))

;; User Directories
(defun user-home (&rest path-segments)
  "Resolves the absolute path formed PATH-SEGMENTS to the
   user home directory."
  (let ((root-separator "/"))
    (--> (getenv "HOME")
         (f-split it)
         (append it path-segments)
         (cdr it)
         (cons (concat root-separator (car it) )(cdr it))
         (apply #'resolve-path it)
         (f-slash it))))

(defalias #'dropbox-dir (apply-partially #'user-home "Dropbox"))
(defalias #'projects-dir
 (apply-partially #'user-home "Developer" "Projects"))
;; Emacs Directories
(defalias #'emacs-dir (apply-partially #'user-home ".emacs.d"))
(defalias #'straight-dir (apply-partially #'emacs-dir "straight"))
(defalias #'emacs-var-dir (apply-partially #'emacs-dir "var"))
(defalias #'emacs-bin-dir (apply-partially #'emacs-dir "bin"))
(defalias #'emacs-etc-dir (apply-partially #'emacs-dir "etc"))
;; Readonly directories
(defvar read-only-directories '()
  "A list of directories for which all files and subdirectories
should open in `read-only-mode'.")

(defvar read-only-whitelist-directories '()
  "A list of directories which should not be opened in read-only mode.")

(defvar read-only-file-extensions '("\\.gz\\'")
  "Not yet implemented.")

(defun dir-level (dir)
  (length (f-split dir)))

(defun dir-or-subdir-p (d1 d2)
  "If non-nil, then directory D1 is subdirectory or the same directory as D2."
  (cond ((< (dir-level d1) (dir-level d2)) nil)
        ((string= d1 d2) t)
        (t (dir-or-subdir-p
            (--> d1 basename file-name-as-directory (string-remove-suffix it d1))
            d2))))

(defun read-only-directory-p (bf)
"Determine whether the current buffer file is in a directory
that is a member of `read-only-directories'."
  (cl-flet ((f (d) (funcall #'dir-or-subdir-p bf d)))
    (and (not (null (seq-filter #'f read-only-directories)))
         (null (seq-filter #'f read-only-whitelist-directories))
         (file-writable-p bf))))

(defun halidom/open-buffer-as-read-only (file)
  "All buffers from `read-only-directories' or
`read-only-file-extensions' are set to read-only."
  (let ((bd (and buffer-file-name (file-name-directory file))))
    (if (read-only-directory-p bd) (read-only-mode 1))))

(defun halidom/find-file-read-only-hook ()
  (funcall #'halidom/open-buffer-as-read-only (buffer-file-name)))

(setq read-only-directories `(,(straight-dir) "/usr/local/"))

(setq read-only-whitelist-directories '("/usr/local/src/llvm-project/"))
(add-hook 'find-file-hook #'halidom/find-file-read-only-hook)

(defun ssl-p ()
  "Determine whether https protocol is supported."
  (and (not (memq system-type '(windows-nt ms-dos)))
       (gnutls-available-p)))

(defun list-enabled-minor-modes (&optional buf)
  "The minor modes enabled in the current buffer."
  (let ((auto-save-mode nil)
        (buf (or buf (current-buffer))))
    (cl-loop for mode being the element of minor-mode-list
             when (boundp mode)
             when (symbol-value mode)
             collect mode)))


(defvar minor-modes-enabled-list (list-enabled-minor-modes (current-buffer))
  "The list of enabled minor modes")

(defun minor-mode-enabled-p (mode)
  (member mode (list-enabled-minor-modes (current-buffer))))

(defun directory-files-no-wildcards (directory &optional full nosort)
   "List directory contents without wildcards"
   (cddr (directory-files directory full nil nosort)))

(defun halidom/rev-up-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun halidom/rev-down-gc ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'halidom/rev-up-gc)
(add-hook 'minibuffer-exit-hook #'halidom/rev-down-gc)

(use-package no-littering
:custom
(no-littering-etc-directory
 (expand-file-name "etc" user-emacs-directory)
 (no-littering-var-directory
  (expand-file-name "var" user-emacs-directory))))

(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(let ((directory (emacs-etc-dir "custom"))
      (file (pcase system-type
              (`darwin "custom-macos.el")
              (`gnu/linux "custom-linux.el")
              (`windows "custom-windows.el"))))
  (setq custom-file (expand-file-name file directory))
  ;; Create custom file if it does not exist.
  (if-not (file-exists-p custom-file)
      (with-temp-buffer
        (find-file custom-file)
        (save-buffer)
        (kill-buffer)))
  (load custom-file))

(setq make-backup-files nil)

(setq create-lockfiles nil)

(defun goto-init ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun goto-literate ()
  (interactive)
  (find-file halidom-user-literate-init-file))

(defun goto-tangled-init ()
  (interactive)
  (find-file
    (concat (file-name-sans-extension halidom-user-literate-init-file) ".el")))


(define-key goto-map "i" #'goto-init)
(define-key goto-map "l" #'goto-literate)
(define-key goto-map "t" #'goto-tangled-init)

(when (and (executable-find "gpg") *is-mac*)
  (if-not (string-empty-p
       (shell-command-to-string
	      (concat "gpg --list-keys | grep " user-mail-address)))
      (progn
        (add-to-list 'load-path (emacs-etc-dir "secrets"))
        (require 'secrets))
    (print (format "GPG key(s) for %s not found"
                   (or user-full-name user-mail-address)))))

(setq-default fill-column 80)

(global-subword-mode 0)
(delete-selection-mode 1)
(global-hl-line-mode -1)
(save-place-mode 1)

(setq load-prefer-newer t)

(setq gnutls-min-prime-bits 4096)

(when *is-mac*
  (let* ((has-brew (not (string-empty-p
			 (shell-command-to-string
			  "which brew"))))
	 (gpg-path (if has-brew
		       (shell-command-to-string "brew --prefix gpg2")))
	 (has-gpg2 (if gpg-path
		             (file-exists-p
                  (replace-regexp-in-string "\n" "" gpg-path)))))
    (setq epg-gpg-program (if has-gpg2 "gpg2" "gpg"))))

(setenv "GPG_AGENT_INFO" nil)

(append completion-ignored-extensions
        '("o" "~" ".lbin" ".so" ".a"
          ".git/" ".hg/" ".svn" ".svn-base"))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Begin Keybindings
;; Which Key
(use-package which-key
  :custom
  (which-key-enable-extended-define-key t)
  :init
  (which-key-mode))
;; Hydra
;; Hydra
(use-package hydra)
;; Ivy Hydra
(use-package ivy-hydra)
;; Smartrep

;; Prefix Map
(define-prefix-command 'halidom-prefix-map)
(global-set-key halidom-prefix 'halidom-prefix-map)
;; Reload Eval
;; Begin reload-eval Group
;; Reload eval straight
;; Reload Init

(defun straight-reload-init (&optional debug)
  "Reload init file using straight transaction system."
  (interactive "P")
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading initialization file...")
    (when debug
      (set (make-local-variable 'use-literate-p) nil)
      (set (make-local-variable 'user-init-file)
	   (expand-file-name "init.el" user-emacs-directory)))
    (load user-init-file nil 'nomessage)
    (message "Reloading initialization file...done.")))


;; Eval Buffer
(defun straight-eval-buffer ()
  "Evaluate current buffer using the straight transaction system."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (or (null buffer-file-name)
            (not (file-exists-p buffer-file-name)))
        (eval-buffer)
      (progn
        (when (string= buffer-file-name user-init-file)
          (straight-mark-transaction-as-init))
        (load-file buffer-file-name))))

  (message "Evaluating %s...done." (buffer-name)))
;; Restart Emacs
(use-package restart-emacs)
;; Reload Eval Keymap

(define-prefix-command 'reload-eval-prefix-map)

(let ((map reload-eval-prefix-map))
  (define-key map "i" 'straight-reload-init)
  (define-key map "b" 'straight-eval-buffer)
  (define-key map "r" 'eval-region)
  (define-key map "f" 'straight-eval-defun)
  (define-key map "q" 'restart-emacs))

(define-key 'halidom-prefix-map (kbd "r") 'reload-eval-prefix-map)
(which-key-add-prefix-title "M-m r" "Reload/Eval")
;; End reload-eval Group
;; Speed Type
(use-package speed-type)
;; End Keybindings

(use-package esup)

(define-prefix-command 'reload-eval-prefix-map)

(let ((map reload-eval-prefix-map))
  (define-key map "i" 'straight-reload-init)
  (define-key map "b" 'straight-eval-buffer)
  (define-key map "r" 'eval-region)
  (define-key map "f" 'straight-eval-defun)
  (define-key map "q" 'restart-emacs))

(define-key 'halidom-prefix-map (kbd "r") 'reload-eval-prefix-map)
(which-key-add-prefix-title "M-m r" "Reload/Eval")

(setq system-uses-terminfo t)

(when *is-mac*
  (set-terminal-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package exec-path-from-shell
  ;; only load `exec-path-from-shell' package on macos and linux.
  :if (memq window-system '(mac ns))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))

(use-package system-packages
:init
 (defun system-packages/update-brew-commands (commands)
   "Update the brew commands supported in system-packages."
     (dolist (command  commands)
       (destructuring-bind (name . cmd) command
         (setf (cdr (assoc
                     name
                     (cdr
                      (assoc
                       'brew
                       system-packages-supported-package-managers))))
               cmd))))

 :config
 (with-eval-after-load 'system-packages
   (let ((commands-alist '((get-info . "brew info")
                          (verify-all-packages . "brew doctor")
                          (log . "brew log"))))
     (system-packages/update-brew-commands commands-alist))))

(use-package prodigy)

;; Begin MacOS
(progn
  ;; MacOS keybindings
  ;; MacOS prefix
  (define-prefix-command 'macos-prefix-map)
  (define-key 'halidom-prefix-map "m" 'macos-prefix-map)
  (which-key-add-prefix-title "M-m m" "macOS")
  ;; Modifer Keys
  (setq mac-command-modifier 'super
        mac-option-modifier  'meta
        ns-control-modifier  'control
        ns-function-modifier 'hyper)
  
  (when *is-mac*
    (global-set-key (kbd "s-=" ) 'text-scale-increase)
    (global-set-key (kbd "s--")  'text-scale-decrease)
    ;; Default is <XF86Back> .. C-x <right>
    (global-set-key (kbd "s-[")  'previous-buffer)
    (global-set-key (kbd "s-]")  'next-buffer)
    (global-set-key (kbd "s-}")  'ns-next-frame)
    (global-set-key (kbd "s-{")  'ns-prev-frame)
    (global-set-key (kbd "s-L")   'mark-sexp))
  
  ;; Mouse-2
  ;; From https://emacs.stackexchange.com/questions/20946/generate-mouse-2-event-from-macbook-trackpadTrackpage
  (when *is-mac*
    (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>")))

  ;; macOS computer name
  (defun halidom/computer-name-cmd ()
    (let* ((has-scutil
            (executable-find "scutil"))
  	       (scutil-cmd
            (lambda ()
              (shell-command-to-string "scutil --get ComputerName"))))
      (if has-scutil
  	      (replace-regexp-in-string "\n" "" (funcall scutil-cmd)) nil)))
  
  (defvar computer-name nil)
  
  (when *is-mac*
    (setq computer-name (halidom/computer-name-cmd)))
  
  (defconst *is-hal* (string= computer-name "hal"))

  ;; macOS finder
  (use-package reveal-in-osx-finder
    :if *is-mac*
    :commands (reveal-in-osx-finder))

  ;; macOS Dash
  (defvar dash-plugin-keywords nil
    "An `alist' of keywords representing the docsets which should
    searched in the query to Dash.app")
  
  (defun macos-dash-at-point ()
    (interactive)
    (let* ((protocol "dash-plugin://")
           (keywords (if dash-plugin-keywords
                         (mapconcat 'identity dash-plugin-keywords ",")
                       ""))
           (search-string
            (if (use-region-p)
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (substring-no-properties (or (thing-at-point 'symbol) "")))))
  
      (start-process "Dash" nil "open"
                     (concat
                      protocol
                      (unless (string-empty-p keywords)
                        (concat "keys=" keywords "&"))
                      "query="
                      (url-hexify-string search-string)))))

  ;; macOS Dev Utils
  (use-package macos-dev-utils
    :straight
    (macos-dev-utils
     :host github
     :repo "jchaffin/macos-dev-utils")
  
    :commands (macos-iterm-command-map macos-open-with-command-map)
    :init
    (require 'macos-dev-utils)
    (which-key-add-key-based-replacements "M-m m t"
        '("iTerm" . "iTerm keys"))
    (which-key-add-key-based-replacements "M-m m o"
        '("Open" . "Open With keys"))
    (define-key 'macos-prefix-map "t" 'macos-iterm-command-map)
    (define-key 'macos-prefix-map "o" 'macos-open-with-command-map))
  

  ;; macOS Dictionary.app
  (use-package osx-dictionary
    :if *is-mac*
    :defines (osx-dictionary-open-dictionary-app-at-point)
    :commands (osx-dictionbary-search-word-at-point
               osx-dictionary-search-input)
    :init
    (progn
      (defun osx-dictionary-open-dictionary-app-at-point ()
        (interactive)
        "Open `word' at point in Dictionary.app."
  
        (shell-command (format "open dict://%s" (thing-at-point 'word))))
      (define-prefix-command 'osx-dictionary-keymap)
      (define-key 'macos-prefix-map
          "d" 'osx-dictionary-keymap)
      (define-key 'osx-dictionary-keymap
          "d" 'osx-dictionary-search-word-at-point)
      (define-key 'osx-dictionary-keymap
          "s" 'osx-dictionary-search-input)
      (define-key 'osx-dictionary-keymap
          "o" 'osx-dictionary-open-dictionary-app-at-point)))
  

  ;; macOS Trash
  (use-package osx-trash
    :if (and *is-mac* (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init
    (progn
      (osx-trash-setup))
    :config
    (progn
      (setq delete-by-moving-to-trash t)))

  ;; macOS clipboard
  (use-package pbcopy
    :if (and *is-mac* (not (display-graphic-p)))
    :init (turn-on-pbcopy))

  ;; macOS app
  (use-package counsel-osx-app
    :if *is-mac*
    :after ivy
    :demand t
    :commands counsel-osx-app
    :config
    (define-key 'macos-prefix-map "a" 'counsel-osx-app)))
;; End MacOS

(with-eval-after-load 'org
  ;; Org Babel
  ;; Org Babel HTTP
  (use-package ob-http
    :after (ob)
    :demand t)
  ;; Org Babel CLJS
  (use-package ob-clojurescript
    :if (executable-find "lumo")
    :after (ob)
    :demand t)
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((C . t)
      (clojure . t)
      (clojurescript . t)
      (dot . t)
      (ditaa . t)
      (latex . t)
      (http . t)
      (perl . t)
      (python . t)
      (plantuml . t)
      (java . t)
      (ruby . t)
      (R . t)
      (shell . t)
      (org . t)))
  ;; Org UI
  ;; Enable visual fill column
  
  (defun halidom/vfc-hook ()
    (visual-fill-column-mode 1)
    (visual-line-mode 1))
  
  
  (add-hook 'org-mode-hook #'halidom/vfc-hook)
  
  
  ;; UI Fixed Width Block
  (setq org-image-actual-width nil)
  ;; UI Org Bullets
  (use-package org-bullets
    :demand t
    :config
    (progn
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode +1)))))
  ;; UI Toc Org
  (use-package toc-org
    :init
    (add-hook 'org-mode-hook 'toc-org-enable))
  ;; UI Column View
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  ;; UI Equation Renumbering
  (defun org-renumber-environment (orig-func &rest args)
    (let ((results '())
          (counter -1)
          (numberp))
  
      (setq results (loop for (begin .  env) in
                          (org-element-map (org-element-parse-buffer)
                          'latex-environment
                            (lambda (env)
                              (cons
                               (org-element-property :begin env)
                               (org-element-property :value env))))
                          collect
                          (cond
                           ((and (string-match "\\\\begin{equation}" env)
                                 (not (string-match "\\\\tag{" env)))
                            (incf counter)
                            (cons begin counter))
                           ((string-match "\\\\begin{align}" env)
                            (prog2
                                (incf counter)
                                (cons begin counter)
                              (with-temp-buffer
                                (insert env)
                                (goto-char (point-min))
                                ;; \\ is used for a new line. Each one leads to a number
                                (incf counter (count-matches "\\\\$"))
                                ;; unless there are nonumbers.
                                (goto-char (point-min))
                                (decf counter (count-matches
                                "\\nonumber")))))
                           (t
                            (cons begin nil)))))
  
      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))
  
    (apply orig-func args))
  
  (advice-add 'org-create-formula-image
              :around #'org-renumber-environment)
  
  ;; Org docmgr
  ;; Begin Org Doc Manager Section
  
  ;; Org Capture
  ;; org capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/todos/TODOs.org" "Tasks")
           "* TODO %?\n %i\n %a")))
  
  (defvar chaffin:created-property-string "
    :PROPERTIES:
    :CREATED: %U
    :END:")
  ;; org protocol handler
  (require 'org-protocol)
  
  (defun org-protocol-handler-install ()
    (interactive)
    (let* ((install-dir (no-littering-expand-var-file-name
                      "org-protocol-handler"))
           (op-app-path (f-join install-dir "org-protocol-handler"
                                "Org Protocol Handler.app"))
           (url-type (if (ssl-p) "https" "http"))
           (git-repo install-dir))
      (unwind-protect
          (if-not (f-dir-p install-dir)
              (progn
                (f-mkdir install-dir)
                (git-clone
                 "https://github.com/aaronbieber/org-protocol-handler.git")
                (f-symlink op-app-path "/Applications/"))))))
  
  (with-eval-after-load 'server
    (unless (server-running-p)
      ;; (setq server-socket-dir (f-expand "server" user-emacs-directory))
      (server-start))
  
  	(if (and *is-mac* (server-running-p))
  	    (org-protocol-handler-install)))
  
  (add-to-list 'org-capture-templates
               '("l" "A link, for reading later." entry
                 (file+headline "notes.org" "Reading List")
                 "* %:description\n%u\n\n%c\n\n%i"
                 :empty-lines 1))
  (use-package org-mac-protocol
      :config
    (defun org-mac-protocol-install ()
      (interactive)
       "Install system dependencies provided by org-mac-protocol."
       (cl-flet ((f (file)
                    (let ((app-dir (no-littering-expand-var-file-name
                                    "org-mac-protocol"))
                         (script-dir (user-home "Library" "Scripts"))
                         (file-path (concat (straight-dir "repos" "org-mac-protocol") file)))
                     (if-not (file-directory-p app-dir)
                         (progn
                           (mkdir app-dir)
                           (message "Created directory %s" app-dir)))
                     (cond ((and (file-directory-p file-path)
                                 (not (file-directory-p
                                       (concat script-dir (file-name-base file-path)))))
                            (copy-directory file-path script-dir))
                           ((string= (file-name-extension file-path) "scpt")
                            (copy-file file-path script-dir t))
                           ((string= (file-name-extension file-path) "applescript")
                            (copy-file file-path app-dir t))))))
         (let ((files (remove ".git" (directory-files-no-wildcards
                                      (straight-dir "repos" "org-mac-protocol")))))
           (mapcar #'f files)))))
  ;; org capture html
  (use-package org-protocol-capture-html
    :straight (org-protocol-capture-html
               :host github
               :repo "alphapapa/org-protocol-capture-html")
    :after (org-capture)
    :init
    (progn
      (add-to-list 'org-capture-templates
                   '("w" "Web site" entry
                     (file "~/Dropbox/org/capture.org")
                     "* %a :website:\n\n%U %?\n\n%:initial")))
    :config
    (setq opch-shell-script-path
          (emacs-bin-dir "org-protocol-capture-html.sh"))
  
    (defun opch-install-shell-script ()
      (interactive)
      (if-not (file-exists-p opch-shell-script-path)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/alphapapa/org-protocol-capture-html/master/org-protocol-capture-html.sh"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (when (re-search-backward "^\\#!/bin/bash" (point-min) t)
              (write-region (point) (point-max) opch-shell-script-path)
              (set-file-modes opch-shell-script-path #o755)
              (message "Installed to %s" opch-shell-script-path))))))
  
  ;; org capture contacts
  (use-package org-contacts
    :straight org
    :config
    (push '("c" "Contacts" entry (file "~/Dropbox/org/contacts.org")
             "* %(org-contacts-template-name)
                :PROPERTIES:
                :EMAIL: %(org-contacts-template-email)
                :END:")
          org-capture-templates))
  ;; Org Journal
  (use-package org-journal
    :if *is-mac*
    ;; :bind
    ;; (("M-m j" . org-journal-new-entry))
    :config
    (progn
      (setq org-journal-dir (concat (file-name-as-directory org-directory) "journal")
            org-journal-date-prefix "#+TITLE: "
            org-journal-date-format "%A, %B %d %Y"
            org-journal-time-prefix "* "
            org-journal-time-format "")
      (push '("j" "Journal" entry
              (file+olp+datetree "~/Dropbox/org/journal.org")
              "* %?\nEntered on %U\n %i\n %a")
            org-capture-templates)))
  ;; Org ID
  
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  
  (defun org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
     If POM is nil, refer to the entry at point. If the entry does
     not have an CUSTOM_ID, the function returns nil. However, when
     CREATE is non nil, create a CUSTOM_ID if none is present
     already. PREFIX will be passed through to `org-id-new'. In any
     case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base id))))))))
  (defun org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
    file which do not already have one. Only adds ids if the `auto-id' option is set a non-nil value in the file.
  
    i.e `#+OPTIONS: auto-id:t`"
  
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^\\#+OPTIONS:.*auto-id:t"  (point-max)  t)
        (org-map-entries
         (lambda ()
           (org-custom-id-get (point) 'create))))))
  
  (add-hook 'org-mode-hook
  	  (lambda ()
  	    (add-hook 'before-save-hook
                  (lambda ()
                    (when (and (eq major-mode 'org-mode)
                               (eq buffer-read-only nil))
                      (org-add-ids-to-headlines-in-file))))))
  ;; Org Download
  (use-package org-download
    :defines (org-download-image-dir)
    :commands (org-download-enable  org-download-yank org-download-screenshot)
    :init
    (progn
      (when *is-mac*
        (setq-default org-download-image-dir "~/Dropbox/org/img/"))
      (add-hook  'org-mode-hook 'org-download-enable)
      (add-hook 'dired-mode-hook 'org-download-enable)))
  ;; Org Links
  ;; Org Tags
   (defun halidom/tag-link (tag)
     "Display a list of TODO headlines with tag TAG.
   With prefix argument, also display headlines without a TODO keyword."
     (org-tags-view (null current-prefix-arg) tag))
   
   (org-add-link-type
    "tag" 'halidom/tag-link)
  ;; Org Man
  (use-package org-man
    :straight org)
  ;; Org Mac LInk
  (use-package org-mac-link
    :straight org)
  ;; Cliplink
  (use-package org-cliplink
    :bind (("C-x p i" . org-cliplink)))
  ;; Org elisp help
  (use-package org-elisp-help
    :straight t)
  ;; Org YouTube
  (org-link-set-parameters
   "youtube"
   :follow (lambda (path)
  	   (browse-url (format "https://youtu.be/%s" path)))
   :export (lambda (path desc backend)
  	   (cond
  	    ((eq 'md backend)
  	     (format "[%s](%s)
  
  <a href=\"https://www.youtube.com/watch?v=%s\">
  <img src=\"http://img.youtube.com/vi/%s/0.jpg\"></a>"
  		     (or desc (format "https://youtu.be/%s" path))
  		     (format "https://youtu.be/%s" path)
  		     path path))))
   :help-echo "A youtube video. Click to open in browser.")
  
  ;; Org links minor mode
  (use-package org-link-minor-mode
    :hook (prog-mode . org-link-minor-mode))
  ;; Org Bookmark
  (use-package org-bookmark-heading)
  
  ;; End Org Doc Manager
  ;; Org taskmgr
  ;; Begin Org Task Manager
  ;; Org Habit
  (use-package org-habit
    :straight nil)
  ;; Org Agenda
  ;; Agenda Diary
  (setq diary-file "~/Dropbox/org/diary.org")
  ;; Agenda global todos
  
  (let ((global-agenda-file (resolve-path
                             org-directory
                             "todos"
                             "TODOs.org")))
    (if (file-exists-p global-agenda-file)
        (add-to-list 'org-agenda-files global-agenda-file)))
  
  ;; Agenda files face
  (defvar-local org-use-level-faces nil)
  (defvar org-level-remap-face nil)
  
  (defun halidom/remap-org-level-faces ()
    "Use minimal foreground face in `org-agenda-files' buffers."
  
    (let ((foreground (face-foreground 'default nil 'default)))
      (unless org-use-level-faces
        (mapcar (lambda (face)
                  (add-to-list 'org-level-remap-face
                               (face-remap-add-relative face
                               :foreground foreground)))
                org-level-faces)
        (setq-local org-use-level-faces t))))
  
  (defun halidom/org-agenda-file-hook ()
    (cond ((org-agenda-file-p) (halidom/remap-org-level-faces))
          (org-use-level-faces (mapcar (lambda (f)
                                         (setq face-remapping-alist
                                               (delq f face-remapping-alist)))
                                       org-level-remap-face))))
  
  (add-hook #'org-mode-hook 'halidom/org-agenda-file-hook)
  
  ;; Agenda view
  (setq org-tags-column 0)
  ;; Org Pomodoro
  (use-package org-pomodoro
    :bind (:map org-mode-map
                ("C-c M-RET p" . org-pomodoro))
    :config
    (progn
      (defalias #'org-pomodoro-path
        (apply-partially #'emacs-dir "etc" "pomodoro"))
  
      (setq org-pomodoro-audio-player "/usr/bin/afplay"
            org-pomodoro-tick-sound
            (org-pomodoro-path "clock-ticking-2.wav")
            ;; Start Settings
            org-pomodoro-start-sound-p t ;; enable starting sound
            org-pomodoro-start-sound-args "--volume 0.08"
            org-pomodoro-start-sound
            (org-pomodoro-path "Victory.wav")
          ;; Finished Settings
            org-pomodoro-finished-sound-args "--volume 0.2"
            org-pomodoro-finished-sound
            (org-pomodoro-path "Waves.wav")
            ;; Short Break Settings
            org-pomodoro-short-break-length 5
            org-pomodoro-short-break-sound-args "--volume 0.2"
            org-pomodoro-short-break-sound org-pomodoro-finished-sound
            ;; Long Break Settings
            org-pomodoro-long-break-length 15
            org-pomodoro-long-break-sound-args "--volume 0.2"
            org-pomodoro-long-break-sound
            (org-pomodoro-path "Complete.wav"))))
  ;; Org Google Calendar
  (use-package org-gcal
      :init
    (require 'secrets)
    (require 'org-gcal)
    :custom
    (org-gcal-file-alist
     '(("jchaffin@g.ucla.edu" . "~/Dropbox/org/todos/gcal/TODOS.org"))))
  ;; Org Counsel Clock
  (use-package counsel-org-clock
    :straight (:host github
                     :repo "akirak/counsel-org-clock")
    :after (:all org-agenda ivy))
  ;; Org Projectile
    (use-package org-projectile
      :after (projectile)
      :commands (org-projectile-files-to-agenda)
      :bind (:map projectile-command-map
  		("C-c p n" . org-projectile-todo-completing-read))
      :init
      (defun org-projectile-files-to-agenda ()
        "Add org-projectile project files to org-agenda."
        (interactive)
        (let* ((todo-filepath
  	      (or (bound-and-true-p org-projectile-per-project-filepath)
  		 "TODOs.org"))
  	     (active-projects
  	      (seq-filter
  	       (lambda (proj)
  		 (file-exists-p (expand-file-name todo-filepath proj)))
  	       projectile-known-projects))
  	     (project-agenda-files
  	      (mapcar
  	       (lambda (proj)
  		 (expand-file-name todo-filepath proj))
  	       active-projects)))
  	(setq org-agenda-files
  	      (append org-agenda-files project-agenda-files))))
  
      (org-projectile-files-to-agenda)
  
      :config
      (progn
        (setq org-projectile-per-project-filepath "TODOs.org")
  
        (org-projectile-per-project)
  
        (setq org-projectile-capture-template
  	    (format "%s%s" "** TODO %?" chaffin:created-property-string))
  
        (add-to-list 'org-capture-templates
  		   (org-projectile-project-todo-entry
  		    :capture-character "l"
  		    :capture-heading "Linked Project TODO"))
  
        (add-to-list 'org-capture-templates
  		   (org-projectile-project-todo-entry
  		    :capture-character "p"))
  
        (setq org-confirm-elisp-link-function nil)))
  
  ;; Org Mru
  (use-package org-mru-clock
    :after (:all org-agenda ivy)
    :demand t
    :bind (("C-c C-x i" . org-mru-clock-in)
           ("C-c C-x C-j" . org-mru-clock-select-recent-task))
    :init
    (progn
      (setq org-mru-clock-how-many 50
            org-mru-completing-read #'ivy-completing-read)))
  ;; End Org Task Manager
  ;; Org ideamgr
  (use-package org-brain
    :if *is-mac*
    :defines (org-brain-path)
    :init
    (setq org-brain-path (concat (file-name-as-directory org-directory) "brain"))
    :config
    (progn
      (setq org-id-track-globally t
            org-brain-visualize-default-choices 'all
            org-brain-title-max-length 12)
      (push '("b" "Brain" plain (function org-brain-goto-end)
              "* %i%?" :empty-lines 1)
            org-capture-templates)))
  (use-package org-drill
    :straight org)
  ;; Org export
  ;; Export Wrapper Block
  (defvar org-export-enabled-backends '()
    "alist of symbols representing enabled `org-mode' export backends")
  
  (defun halidom/org-export-enabled-backend-p (backend)
    (member backend org-export-enabled-backends))
  
  ;; Enabled on MacOS block
  (when *is-mac*
    (mapcar (lambda (backend)
              (setq org-export-enabled-backends
                    (cons backend org-export-enabled-backends)))
            '(extra gfm latex hugo html pandoc linguistics)))
  
  ;; Enable Extras
  (use-package ox-extra
    :straight org
    :demand t
    :config
    (ox-extras-activate '(ignore-headlines
                          org-export-filter-parse-tree-functions)))
  
  (use-package ox-publish
    :straight org
    :after (ox)
    :demand t
    :bind (("M-m r p" . org-publish-project)))
  
  ;; LaTeX backend
  ;; Only evaluate LaTeX package configurations if export dispatcher is enabled
  (when (halidom/org-export-enabled-backend-p 'latex)
  
    ;; LaTeX Configuration
    ;; AucTeX
    (use-package auctex
      :bind (:map LaTeX-mode-map
                  ("M-s l" . TeX-engine-set)))
    ;; Tex
      (use-package tex
        :straight auctex
        :custom
        ;; (TeX-command-default
        ;;  (if (executable-find "latexmk") "LatexMk" "LaTeX"))
        (TeX-auto-save t)
        (TeX-parse-self t)
        (TeX-syntactic-comment t)
        ;; nonstopmode
        (TeX-interactive-mode nil)
        ;; Don't insert line-break at inline math
        (LaTeX-fill-break-at-separators nil)
        :init
    
        ;; https://emacs.stackexchange.com/a/19475
        (defun latex/view-hook ()
          "Use pdf-tools to open PDF files."
          (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view))
          (setf (alist-get 'output-pdf TeX-view-program-selection) '("PDF Tools"))
          (setq TeX-source-correlate-start-server t)
          (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
    
        (defun latex/shell-escape ()
          "Use the shell escape flag with `TeX-command'."
          (setq TeX-command-extra-options "-shell-escape"))
    
        (defun latex/enable-flyspell ()
          (when (fboundp 'flyspell-mode)
            (flyspell-mode 1)))
    
        (defun latex/revert-doc-view ()
          (when (fboundp 'doc-view-mode)
            (add-hook 'doc-view-mode 'auto-revert-mode)))
    
        (defun latex/enable-ggtags ()
          (when (fboundp 'ggtags-mode)
            (ggtags-mode 1)))
    
        :hook
        (LaTeX-mode . latex/shell-escape)
        (LaTeX-mode . latex/view-hook)
        (LaTeX-mode . latex/enable-flyspell)
        (LaTeX-mode . latex/revert-doc-view)
        (LaTeX-mode . latex/enable-ggtags)
        (LaTeX-mode . LaTeX-math-mode)
        (LaTeX-mode . TeX-fold-mode)
        (LaTeX-mode . TeX-source-correlate-mode)
        (LaTeX-mode . TeX-PDF-mode))
    ;; Reftex
    (use-package reftex
        :straight nil
        :init
        (defun halidom/latex-reftex-hook ()
          (turn-on-reftex)
          (setq reftex-plug-into-AUCTeX '(nil nil t t t)
                reftex-use-fonts t
                reftex-default-bibliography (list (file-truename "~/Dropbox/org/papers/references.bib"))))
        :hook
        (LaTeX-mode . halidom/latex-reftex-hook))
    ;; Bibtex
    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5)
    ;; Company backend for latex completion
    (use-package company-auctex
      :demand t
      :after (:all company tex)
      :init
      (company-auctex-init))
    (use-package company-reftex
      :demand t
      :after (:all company reftex))
    ;; Prettify latex buffers
    (use-package magic-latex-buffer
        :custom
        (magic-latex-enable-block-highlight t)
        (magic-latex-enable-suscript nil)
        (magic-latex-enable-pretty-symbols t)
        (magic-latex-enable-block-align nil)
        (magic-latex-enable-inline-image nil)
        :hook
        (LaTeX-mode . magic-latex-buffer))
    ;; Extra latex utilities
    (use-package latex-extra
      :custom
      (latex/no-fill-environments
       '("equation"
         "equation*"
         "align"
         "align*"
         "forest"
         "forest*"
         "tabular"
         "tikzpicture"))
    
        :bind (:map LaTeX-mode-map
                    ("C-c C-a" . latex/compile-commands-until-done)
                    ("C-c C-n" . latex/next-section)
                    ("C-c C-u" . latex/up-section)
                    ("C-c C-f" . latex/next-section-same-level)
                    ("C-M-f"   . latex/forward-environment)
                    ("C-M-b"   . latex/backward-environment)
                    ("C-M-a"   . latex/beginning-of-environment)
                    ("C-c C-p" . latex/previous-section)
                    ("C-c C-b" . latex/previous-section-same-level)
                    ("C-c C-q" . latex/clean-fill-indent-environment))
        :hook
        (LaTeX-mode . latex-extra-mode))
    
    ;; Cdlatex
    (use-package cdlatex
      :custom
      ;; Disable auto label insertion in expanded template.
      ;; Labels conflict when used in conjunction with `org-ref'
      (cdlatex-insert-auto-labels-in-env-templates nil)
      :hook
      ;; with AucTeX LaTeX mode
      ;; (LaTeX-mode . turn-on-cdlatex)
      ;; with Emacs latex mode
      ;; (latex-mode . turn-on-cdlatex)
      (org-mode . org-cdlatex-mode))
    ;; Auctex Latexmk
    (use-package auctex-latexmk
        :custom
        (auctex-latexmk-inherit-TeX-PDF-mode t)
        :init
        (auctex-latexmk-setup))
    ;; LaTeX Math
    ;; Math Symbols
    (use-package math-symbols
      :init
    
      (defun latex/font (math-input before after)
       "Insert a LaTeX font. 
    
     MATH-INPUT is the input method to toggle in math environments.
    
     The BEFORE and AFTER arguments are the strings to insert around 
     the marked region, when active. If the thing at point is a word,
     then wrap the word. Otherwise the two elements are joined at point."
       (cond ((texmathp)
              (if current-input-method
                  (set-input-method nil)
                (set-input-method math-input)))
             ((region-active-p)
              (save-excursion
                (goto-char (region-beginning))
                (insert before)
                (goto-char (region-end))
                (insert after)))
             ((thing-at-point 'word)
              (save-excursion
                (goto-char (beginning-of-thing 'word))
                (insert before)
                (goto-char (end-of-thing 'word))
                (insert after)))
             (t
              (save-excursion
                (insert (concat before after)))
              (goto-char (+ (point) (length before))))))
    
      (defun latex/prettify-symbols-extra ()
        (if osx-browse-mode
            (osx-browse-mode nil))
    
        (when (fboundp 'prettify-symbols-mode)
          (prettify-symbols-mode 1))
    
        (setq prettify-symbols-alist
              '(("\\complement" . "∁"))))
    
      (setq-default abbrev-mode t)
      (setq latex-mode-abbrev-table nil)
    
      (define-abbrev-table 'latex-mode-abbrev-table
          '(("uiff"  "⟷")
            ("uif"  "⟶")
            ("uand" "∧")
            ("ulambda" "𝜆")
            ("uor" "∨")
            ("uxor" "⨁")
            ("uexists" "∃")
            ("unexists" "∄")
            ("usubset" "⊂")
            ("unsubset" "⊄")))
    
      (defun latex/unbind-osx-browse ()
        (when (fboundp 'osx-browse-mode)
          (let ((map osx-browse-mode-map))
            (define-key map (kbd "s-b") nil)
            (define-key map (kbd "s-i") nil))))
    
      (defun latex/font-italic ()
        "Insert a literal italic typeset command, or toggle input method
    in math environments with unicode math enabled."
        (interactive)
        (latex/font "math-symbols-italic" "\\textit{" "}"))
    
      (defun latex/font-bold ()
        "Insert a literal bold typeset command, or toggle input method
    in math environments with unicode math enabled."
        (interactive)
        (latex/font "math-symbols-bold" "\\textbf{" "}"))
    
      (let ((map LaTeX-mode-map))
        (define-key map (kbd "s-b") 'latex/font-bold)
        (define-key map (kbd "s-i") 'latex/font-italic))
    
      :hook
      (LaTeX-mode . latex/unbind-osx-browse)
      (LaTeX-mode . latex/prettify-symbols-extra))
    
    ;; Company Math
    (use-package company-math
      :after (company)
      :demand t
      :init
      (add-to-list 'company-backends 'company-math-symbols-unicode))
    ;; Latex Preview
    (use-package latex-preview-pane
        :after (:all pdf-tools tex)
        :init (latex-preview-pane-enable))
    (defun org-preview-clear-cache ()
      (interactive)
      (let ((preview-cache
             (f-join default-directory org-preview-latex-image-directory)))
        (if (f-directory? preview-cache)
            (f-delete preview-cache t)
          (message "%s" "Directory 'ltximg' does not exist."))))
    (setq org-format-xelatex-header
          "\\documentclass{article}
            \\usepackage[usenames]{xcolor}
            [PACKAGES]
            [NO-DEFAULT-PACKAGES]
            \\pagestyle{empty}             % do not remove
            \\usepackage{amsmath}
            \\usepackage{fontspec}
            \\usepackage{unicode-math}
            \\setromanfont[Ligatures=TeX]{Latin Modern Roman}
            \\setmathfont[math-style=ISO,bold-style=ISO]{Latin Modern Math}
            % The settings below are copied from fullpage.sty
            \\setlength{\\textwidth}{\\paperwidth}
            \\addtolength{\\textwidth}{-3cm}
            \\setlength{\\oddsidemargin}{1.5cm}
            \\addtolength{\\oddsidemargin}{-2.54cm}
            \\setlength{\\evensidemargin}{\\oddsidemargin}
            \\setlength{\\textheight}{\\paperheight}
            \\addtolength{\\textheight}{-\\headheight}
            \\addtolength{\\textheight}{-\\headsep}
            \\addtolength{\\textheight}{-\\footskip}
            \\addtolength{\\textheight}{-3cm}
            \\setlength{\\topmargin}{1.5cm}
            \\addtolength{\\topmargin}{-2.54cm}
    
            % some defaults based on personal preference
            \\usepackage{adjustbox}
            \\usepackage[linguistics,edges]{forest}")
    
    ;;; XeTex preview
    (setq org-preview-xemagick
          '(xemagick
            :programs ("xelatex" "convert")
            :description ("xelatex" "convert")
            :message "You need to install xelatex and imagemagick"
            :use-xcolor t
            :image-input-type "pdf"
            :image-output-type "png"
            :image-size-adjust (1.0 . 1.0)
            :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
            :image-converter  ("convert -density %D -trim -antialias %f -quality 200 %O")))
    
    
    (add-to-list 'org-preview-latex-process-alist org-preview-xemagick)
    
    (defun org-preview-xemagick-fragments ()
      (org-element-map (org-element-parse-buffer) 'keyword
        (lambda (keyword)
          (if (string= "xelatex" (org-element-property :value keyword))
              (progn
                (message "%s\n" "Using xelatex format header")
                (set (make-local-variable 'org-format-latex-header)
                     org-format-xelatex-header)
                (set (make-local-variable 'org-preview-latex-default-process)
                     'xemagick)))) nil t))
    
    (add-hook 'org-mode-hook #'org-preview-xemagick-fragments)
    (setq org-preview-luamagick
          '(luamagick
            :programs ("lualatex" "convert")
            :description "pdf > png"
            :message "you need to install lualatex and imagemagick."
            :use-xcolor t
            :image-input-type "pdf"
            :image-output-type "png"
            :image-size-adjust (1.0 . 1.0)
            :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
            :image-converter ("convert -density %D -trim -antialias %f -quality 200 %O")))
    
    (add-to-list 'org-preview-latex-process-alist org-preview-luamagick)
    
    (defun org-preview-luamagick-fragments ()
      (org-element-map (org-element-parse-buffer) 'keyword
        (lambda (keyword)
          (if (string= "uclacs" (org-element-property :value keyword))
              (progn
                (message "%s\n" "Using custom preview header for uclacs LaTeX class.")
                (set (make-local-variable 'org-format-latex-header)
                     org-format-lualatex-header)
                (set (make-local-variable 'org-preview-latex-default-process)
                     'luamagick)))) nil t))
    
    (setq org-format-lualatex-header
          "\\documentclass{article}
            \\usepackage[usenames,dvipsnames,svgnames]{xcolor}
            [PACKAGES]
            [DEFAULT-PACKAGES]
            \\pagestyle{empty}             % do not remove
            \\usepackage{amsmath}
            \\usepackage{fontspec}
            \\usepackage{unicode-math}
            \\usepackage{hologo}
            \\setmathfont{STIX2Math}[
              Extension={.otf},
              Scale=1]
            \\setmainfont{STIX2Text}[
              Extension={.otf},
              UprightFont={*-Regular},
              BoldFont={*-Bold},
              ItalicFont={*-Italic},
            BoldItalicFont={*-BoldItalic}]
            % The settings below are copied from fullpage.sty
            \\setlength{\\textwidth}{\\paperwidth}
            \\addtolength{\\textwidth}{-3cm}
            \\setlength{\\oddsidemargin}{1.5cm}
            \\addtolength{\\oddsidemargin}{-2.54cm}
            \\setlength{\\evensidemargin}{\\oddsidemargin}
            \\setlength{\\textheight}{\\paperheight}
            \\addtolength{\\textheight}{-\\headheight}
            \\addtolength{\\textheight}{-\\headsep}
            \\addtolength{\\textheight}{-\\footskip}
            \\addtolength{\\textheight}{-3cm}
            \\setlength{\\topmargin}{1.5cm}
            \\addtolength{\\topmargin}{-2.54cm}
            % The settings below are copied from ucla.cls
    
            \\usepackage[algoruled,linesnumbered]{algorithm2e}
            \\SetKwInOut{Input}{Input}
            \\SetKwInOut{Output}{Output}
            \\SetKwProg{proc}{Procedure}{}{}
            \\newcommand{\\forcond}[3]{$#1=#2$ \\KwTo $#3$}
            \\newcommand{\\forcondi}[2]{\\forcond{i}{#1}{#2}}
            \\newcommand{\\forcondj}[2]{\\forcond{j}{#1}{#2}}
            \\usepackage{etoolbox}
             \\usepackage{adjustbox}
             \\usepackage{forest}
             \\forestset{
               default preamble={%
                 for tree={circle,draw, l sep=2mm}%
               }%
             }%")
    
    
    (add-to-list 'org-preview-latex-process-alist org-preview-luamagick)
    
    (defun org-preview--complement-bg ()
      (with-eval-after-load 'color
        (substring
           (apply #'color-rgb-to-hex
             (color-complement
          	   (frame-parameter nil 'background-color))) 0 7)))
    
    ;; (setq org-format-latex-options
    ;;       (plist-put org-format-latex-options
    ;;                  :background nil))
    
    ;; (setq org-format-latex-options
    ;;       (plist-put org-format-latex-options
    ;;                  :foreground (org-preview--complement-bg)))
    
    ;; specify the justification you want
    (require 'ov)
    (plist-put org-format-latex-options :justify 'center)
    
    (defun org-justify-fragment-overlay (beg end image imagetype)
      "Adjust the justification of a LaTeX fragment.
    The justification is set by :justify in
    `org-format-latex-options'. Only equations at the beginning of a
    line are justified."
      (cond
       ;; Centered justification
       ((and (eq 'center (plist-get org-format-latex-options :justify))
             (= beg (line-beginning-position)))
        (let* ((img (create-image image 'imagemagick t))
               (width (car (image-size img)))
               (offset (floor (- (/ (window-text-width) 2) (/ width 2)))))
          (overlay-put (ov-at) 'before-string (make-string offset ? ))))
       ;; Right justification
       ((and (eq 'right (plist-get org-format-latex-options :justify))
             (= beg (line-beginning-position)))
        (let* ((img (create-image image 'imagemagick t))
               (width (car (image-display-size (overlay-get (ov-at) 'display))))
               (offset (floor (- (window-text-width) width (- (line-end-position) end)))))
          (overlay-put (ov-at) 'before-string (make-string offset ? ))))))
    
    (defun org-latex-fragment-tooltip (beg end image imagetype)
      "Add the fragment tooltip to the overlay and set click function to toggle it."
      (overlay-put (ov-at) 'help-echo
                   (concat (buffer-substring beg end)
                           "mouse-1 to toggle."))
      (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1]
                                          `(lambda ()
                                             (interactive)
                                             (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                        map)))
    
    ;; advise the function to a
    ;; (advice-add 'org--format-latex-make-overlay :after 'org-justify-fragment-overlay)
    ;; (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)
    ;; That is it. If you get tired of the advice, remove it like this:
    
    ;; (advice-remove 'org--format-latex-make-overlay 'org-justify-fragment-overlay)
    ;; (advice-remove 'org--format-latex-make-overlay 'org-latex-fragment-tooltip)
    ;; Texinfo mode
    (use-package texinfo
      :defines texinfo-section-list
      :commands texinfo-mode
      :mode
      ("\\.texi\\'" . texinfo-mode))
    ;; End LaTeX Config
  
    ;; Org LaTeX
    (use-package org-edit-latex
      :straight t)
    (setq org-highlight-latex-and-related '(latex))
    ;; Org Ref
    (use-package org-ref
      :after ivy
      :demand t
      :custom
    
      (org-ref-completion-library 'org-ref-ivy-cite)
      (org-ref-default-bibliography
       (list (dropbox-dir "org" "ref" "references.bib")))
      (org-ref-bibliography-notes
       (dropbox-dir "org" "ref" "refnotes.org"))
      (org-ref-pdf-directory
       (dropbox-dir "org" "papers" "pdfs"))
      (org-ref-show-citation-on-enter t)
      (org-ref-show-broken-links t)
      (org-ref-bibtex-hydra-key-binding "\C-cj")
    
      :init
      ;; https://github.com/jkitchin/org-ref/issues/428
      (progn
        (require 'doi-utils)
        (require 'org-ref-url-utils)
        (require 'org-id)
        (require 'org-ref-latex)
        (require 'org-ref-bibtex)
        (require 'org-ref-pdf)
        (require 'org-ref-scopus)
        (require 'org-ref-isbn)
        (require 'org-ref-pubmed)
        (require 'org-ref-arxiv)
        (require 'org-ref-sci-id)
        (require 'org-ref-wos)
        (require 'org-ref-worldcat)
        (require 'x2bib))
    
      :config
    
      (defun goto-org-ref-manual (&optional path)
        (interactive)
        (if-not path
            (setq path (emacs-dir "straight" "repos" "org-ref" "org-ref.org")))
        (find-file path))
    
    
      ;; Hydra
      ;; See [[file:straight/repos/org-ref/org-ref.org::#citations][Citations]] section of the manual
      (key-chord-define-global "kk" 'org-ref-cite-hydra/body)
    
      ;; Org Ref Bibtex Interface
      (defcustom halidom-org-ref-journal-abbrevs '()
        "Extra abbreviations to add to `org-ref-bibtex-journal-abbreviations.'
    Each abbreviation is a list of the form (string journal-full-name journal-abbreviation)."
        :type '(alist :value-type (group string)))
    
    
      (setq hist109B-journal-abbrevs
            '(("JPS" "Journal of Palestinian Studies")
              ("JMEISA" "Journal of Middle Eastern and Islamic Studies (in Asia) Vol")
              ("JHG" "Journal of Historical Geography")
              ("MEAST" "International Journal of Middle East Studies")))
    
      (setq halidom-org-ref-journal-abbrevs
            (seq-concatenate 'list
                             halidom-org-ref-journal-abbrevs
                             hist109B-journal-abbrevs))
    
    
    
      (with-eval-after-load 'org-ref
        (dolist (elt halidom-org-ref-journal-abbrevs)
          (add-to-list 'org-ref-bibtex-journal-abbreviations elt))))
    
    
    (with-eval-after-load 'org-ref
      ;; Org ref vref
      (defun org-ref-vref-export (keyword desc format)
        "An export function for vref links."
        (cond
         ((eq format 'html)
          (format "<a href=\"#%s\">%s</a>" keyword (or desc keyword)))
         ((eq format 'latex) (format "\\vref{%s}" keyword))))
      
      
      (org-ref-link-set-parameters "vref"
         :follow #'org-ref-ref-follow
         :export #'org-ref-vref-export
         :complete #'org-ref-complete-link
         :face 'org-ref-ref-face-fn
         :help-echo #'org-ref-ref-help-echo)
      
      (setq org-ref-ref-types '("ref" "eqref" "pageref" "nameref"
                                "autoref" "cref" "Cref" "vref"))
      
    
      ;; Org ref pdf
      (defun org-ref-open-pdf-at-point ()
        "Open the pdf for bibtex key under point if it exists."
        (interactive)
        (let* ((results (org-ref-get-bibtex-key-and-file))
               (key (car results))
               (pdf-file (funcall org-ref-get-pdf-filename-function key)))
          (if (file-exists-p pdf-file)
              (find-file pdf-file)
            (message "No PDF found for %s" key))))
      
      (setq org-ref-open-pdf-function 'org-ref-open-pdf-at-point)
    )
    (when (display-graphic-p)
      (use-package pdf-tools
        :mode (("\\.pdf\\'" . pdf-view-mode))
        :bind (:map pdf-view-mode-map
                    ("C-s" . isearch-forward)
                    ("h"   . pdf-annot-add-highlight-markup-annotation)
                    ("t"   . pdf-annot-add-text-annotation)
                    ("D"   . pdf-annot-delete))
        :init
        (pdf-tools-install)
        :config
        (progn
          (setq-default pdf-view-display-size 'fit-page)
          (setq pdf-annot-activate-created-annotations t)
          (setq pdf-view-resize-factor 1.1)))
      (use-package org-pdfview
        :after (:all pdf-tools)
        :demand t
        :config
        (progn
          (add-hook 'org-mode-hook
                    (lambda ()
                      (add-to-list 'org-file-apps
                                   '("\\.pdf\\'" .
                                     (lambda (file link)
                                       (org-pdfview-open link))))))))
    )
    ;; End Org Latex Expansion
  
    ;; Ox Latex
    (require 'ox-latex)
    
    (setq org-latex-prefer-user-labels t)
    (eval-and-compile
      (defvar enable-default-minted nil))
    
    (setq org-latex-listings 'minted)
    
    (defun latex-toggle-default-minted ()
      (interactive)
      (if enable-default-minted
          (progn
            (setq org-latex-packages-alist '(("" "minted"))
                org-latex-minted-options
                '(("mathescape" "true")
                  ("linenos" "true")
                  ("breaklines" "true")
                  ("numbersep" "5pt")
                  ("frame" "lines")
                  ("framesep" "2mm")))
            (setq enable-default-minted nil)
            (message "%s" "Exporting with default minted."))
        (progn
          (setq org-latex-packages-alist '()
                org-latex-minted-options '())
          (setq enable-default-minted t)
          (message "%s" "Disabled default minted."))))
    
    (setq latex-process-latex
      '("%latex -interaction nonstopmode -output-directory %o %f"
        "%latex -interaction nonstopmode -output-directory %o %f"
        "%latex -interaction nonstopmode -output-directory %o %f"))
    
    (setq latex-process-pdflatex
      '("latexmk -pdflatex='pdflatex -interaction=nonstopmode -shell-escape' -synctex=1 -pdf -bibtex -f %f"))
    
    (setq latex-process-xelatex
      '("latexmk -pdf -synctex=1 -shell-escape -xelatex -f %f"))
    
    (setq latex-process-lualatex
      '("latexmk -pdf -synctex=1 -shell-escape -lualatex -f %f"))
    
    (defcustom halidom-latex-pdf-engines
      '(("latex" . latex-process-latex)
        ("lualatex" . latex-process-lualatex)
        ("xelatex" . latex-process-xelatex)
        ("pdflatex" . latex-process-pdflatex))
      "A list of LaTeX commands available to run when 
    `org-latex-export-to-pdf' is invoked."
      :type '(cons string symbol))
    
    (setq org-latex-pdf-process latex-process-xelatex)
    
    (defvar org-latex-pdf-process-set-hook nil)
    
    (defun org-latex-pdf-process-set (&optional process)
      (interactive)
      (let* ((process (or
                       process
                       (assoc (completing-read "Process: " halidom-latex-pdf-engines nil nil)
                              halidom-latex-pdf-engines)))
             (cmd-string (cdr process)))
        (setq org-latex-pdf-process (symbol-value cmd-string))
        (run-hooks 'org-latex-pdf-process-set-hook)))
    
    
    (add-hook 'org-mode-hook
              (lambda () (local-set-key (kbd "M-s l") 'org-latex-pdf-process-set)))
    
    (defvar org-latex-pdf-process-set-hook nil)
    
    (if (and (executable-find "kpsewhich")
             (shell-command-to-string "kpsewhich orgling.cls"))
    
        (add-to-list 'org-latex-classes
                     '("orgling"
                       "\\documentclass{orgling}
                        [NO-DEFAULT-PACKAGES]
                        [EXTRA]"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    
    (add-to-list 'org-latex-classes
                 '("uclacs"
                   "\\documentclass{uclacs}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("humanities"
                   "\\documentclass{humanities}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    
    
  ) ;; End LaTeX Backend
  
  ;; BibTeX backend
  (when (halidom/org-export-enabled-backend-p 'bibtex)
    (use-package ox-bibtex
      :straight org
      :mode
      (("\\.org.bib\\'" . org-mode))
      :after (ox)
      :demand t
      :config
      (progn
        (require 'org-bibtex)
        (setq org-bibtex-file "references.org"))))
  
  ;; GFM backend
  (when (halidom/org-export-enabled-backend-p 'gfm)
    (use-package ox-gfm
      :after (ox)
      :demand t))
  
  ;; Pandoc backend
  (when (halidom/org-export-enabled-backend-p 'pandoc)
    (use-package ox-pandoc
      :if (executable-find "pandoc")
      :after (:all ox org-ref)
      :custom
    
      (org-pandoc-options '((standalone . t)))
    
      :demand t
      :config
      (progn
    
        ;; LaTeX-Beamer-PDF
        (defun ox-pandoc--pdf-engine ()
          "Set the default latex pdf engine to the one set by `org-latex-pdf-process'. "
          (let ((syms (mapcar (lambda (x) (cdr x)) halidom-latex-pdf-engines))
                (pred (lambda (sym) (eq (symbol-value sym) org-latex-pdf-process)))
                (sep "latex-process-"))
            (cadr (split-string (symbol-name (car (seq-filter pred syms))) sep))))
    
        (setq org-pandoc-options-for-beamer-pdf
              `((pdf-engine . ,(ox-pandoc--pdf-engine)))
              org-pandoc-options-for-latex-pdf
              `((pdf-engine . ,(ox-pandoc--pdf-engine))))
    
        (defun org-pandoc-pdf-engine-set (&optional process)
          "Set the latex pdf engine for `org-pandoc-export-to-latex-pdf'."
          (interactive)
          (let* ((process (or process
                              (assoc
                               (completing-read "Pandoc Process: "
                                                halidom-latex-pdf-engines nil nil)
                               halidom-latex-pdf-engines)))
                 (sym (substring-no-properties (car process))))
            (setq org-pandoc-options-for-beamer-pdf
                  `((pdf-engine . ,sym))
                  org-pandoc-options-for-latex-pdf
                  `((pdf-engine . ,sym)))))
    
        ;; Open MS .doc?x files with system viewer.
        (when (symbolp 'org-file-apps)
          (add-to-list 'org-file-apps '("\\.docx?\\'" . system)))))
  )
  
  ;; Hugo backend
  (when (halidom/org-export-enabled-backend-p 'hugo)
    (use-package ox-hugo
      :after (ox))
  )
  
  ;; HTML Backend
  (when (halidom/org-export-enabled-backend-p 'html)
    (straight-use-package
     `(org-html-themes
       :host github
       :repo "fniessen/org-html-themes"
       :local-repo-name org-html-themes
       :files ("setup/*" "styles/*")))
  )
  
  ;; OX Linguistics
  (when (halidom/org-export-enabled-backend-p 'linguistics)
    (use-package ox-linguistics
        :straight (ox-linguistics
                   :host github
                   :repo "wyleyr/ox-linguistics"
                   :files ("lisp/*.el"))
        :after (ox)
        :demand t)
    )
)

;; Begin Org Hacks
;; Hidden cursor in folded subtree fix
(add-hook 'org-mode-hook
          (lambda ()
            (when (outline-invisible-p)
              (save-excursion
                (outline-previous-visible-heading 1)
                (org-show-subtree)))))
;; Extract hyperlinks

(defun org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))


(defun browse-url-extract-org-link (orig-fun &rest args)
  "If `thing-at-point' is a org-link, then call `org-extract-link'
and apply ORIG-FUN with the extracted url in the car of original ARGS."
  (when (and (eq major-mode 'org-mode)
             (string= (car (org-thing-at-point)) "link"))
    (setcar args (org-extract-link)))
  (apply orig-fun args))

  (advice-add 'osx-browse-url :around #'browse-url-extract-org-link)

;; Dynamically adjust tag position
(setq ba/org-adjust-tags-column nil)

(defun ba/org-adjust-tags-column-reset-tags ()
  "In org-mode buffers it will reset tag position according to
`org-tags-column'."
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (let ((b-m-p (buffer-modified-p)))
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (command-execute 'outline-next-visible-heading)
            ;; disable (message) that org-set-tags generates
            (cl-flet ((message (&rest ignored) nil))
            (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
            (error nil)))))

(defun ba/org-adjust-tags-column-now ()
  "Right-adjust `org-tags-column' value, then reset tag position."
  (set (make-local-variable 'org-tags-column)
       (- (- (window-width) (length org-ellipsis))))
  (ba/org-adjust-tags-column-reset-tags))

(defun ba/org-adjust-tags-column-maybe ()
  "If `ba/org-adjust-tags-column' is set to non-nil, adjust tags."
  (when ba/org-adjust-tags-column
    (ba/org-adjust-tags-column-now)))

(defun ba/org-adjust-tags-column-before-save ()
  "Tags need to be left-adjusted when saving."
  (when ba/org-adjust-tags-column
     (setq org-tags-column 1)
     (ba/org-adjust-tags-column-reset-tags)))

(defun ba/org-adjust-tags-column-after-save ()
  "Revert left-adjusted tag position done by before-save hook."
  (ba/org-adjust-tags-column-maybe)
  (set-buffer-modified-p nil))

; automatically align tags on right-hand side
(add-hook 'window-configuration-change-hook
          'ba/org-adjust-tags-column-maybe)
(add-hook 'before-save-hook 'ba/org-adjust-tags-column-before-save)
(add-hook 'after-save-hook 'ba/org-adjust-tags-column-after-save)
(add-hook 'org-agenda-mode-hook '(lambda ()
                                  (setq org-agenda-tags-column (- (window-width)))))

; between invoking org-refile and displaying the prompt (which
; triggers window-configuration-change-hook) tags might adjust,
; which invalidates the org-refile cache
(defadvice org-refile (around org-refile-disable-adjust-tags)
  "Disable dynamically adjusting tags"
  (let ((ba/org-adjust-tags-column nil))
    ad-do-it))
(ad-activate 'org-refile)
;; End Org Hacks

(setq org-latex-hyperref-template "\\hypersetup{\n colorlinks=true, urlcolor=black,linkcolor=black \n}")

(use-package projectile
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :custom
    (project,ile-completion-system 'ivy)
    (projectile-switch-project-action #'projectile-dired)
    (projectile-find-dir-includes-top-level t)
    (projectile-indexing-method 'alien)
    (projectile-enable-caching t)

    :init
    (el-patch-feature projectile)

    (el-patch-defun projectile-run-compilation (cmd)
      "Run external or Elisp compilation command CMD."
      (if (functionp cmd)
          (funcall cmd)
        (compile cmd (el-patch-add t))))

    :config

    (progn

      (append projectile-globally-ignored-directories
              '("gradle" "target" ".meghanada"
                ".gradle" "build" "bin" "node_modules"
                "CMakeFiles" ".cquery_cached_index"))

      (defvar halidom/ignored-project-directories
        '("~/.emacs.d/straight"))

      (defun projectile-ignore-projects-in-directory (project-root)
        (cl-flet ((ignored-dir-or-subdir-p (path)
                    (f-descendant-of?
                     (f-expand project-root)
                     path)))
          (->> halidom/ignored-project-directories
             (seq-filter #'ignored-dir-or-subdir-p)
             seq-empty-p not)))
      (setq projectile-ignored-project-function
            #'projectile-ignore-projects-in-directory)))

(use-package projectile-codesearch)

(use-package skeletor
    :custom
    (skeletor-user-directory (emacs-dir "skeletons"))
    (skeletor-completing-read-function 'ivy-completing-read)
    (skeletor-python-bin-search-path '("/usr/local/bin" "/usr/bin"))
    (skeletor-project-directory (projects-dir))

    :init

    (defun skeletor-add-pyenv-pythons ()
      "Add python binaries managed by pyenv to `skeletor-python-bin-search-path'."
      (let* ((pyenv-dir (getenv "PYENV_ROOT"))
             (pyenv-version-dir (if pyenv-dir (resolve-path pyenv-dir "versions")))
             (pyenv-versions (directory-files-no-wildcards pyenv-version-dir t))
             (python-bins (mapcar (lambda (d) (resolve-path d "bin")) pyenv-versions)))
        (dolist (python-bin python-bins)
          (add-to-list 'skeletor-python-bin-search-path python-bin))))

    (skeletor-add-pyenv-pythons)


    :config

    (add-to-list 'skeletor-global-substitutions (cons "__TIME__" (lambda () (format-time-string "%c"))))

    (skeletor-define-template "cmake-unix-makefiles"
      :requires-executables '(("cmake" . "https://cmake.org")
                              ("make". "https://www.gnu.org/software/make"))
      :substitutions
      (list (cons "__DESCRIPTION__" (lambda () (read-string "Description: ")))
            (cons "__TARGET_NAME__"  (lambda () (read-string "Target: ")))
            (cons "__PROJECT-VARS__" ".dir-locals"))

      :after-creation
      (lambda (dir)
        (skeletor-async-shell-command "mkdir build")
        (skeletor-async-shell-command  "( cd build && cmake -G 'Unix Makefiles' -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .. )")
        (skeletor-async-shell-command "ln -s `pwd`/build/compile_commands.json `pwd`/compile_commands.json")
        (dired dir)
        (revert-buffer)))



    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-project-blacklist skeletor-project-directory))

    (with-eval-after-load 'projectile
      (add-to-list 'projectile-ignored-projects skeletor-project-directory)))

(use-package find-file-in-project)

(use-package async
  :after dired
  :commands (dired-async-mode async-smtpmail-send-it)
  :init
  (with-eval-after-load 'async
    (require 'smtpmail-async)
    (setq message-send-mail-function 'async-smtpmail-send-it)
    (dired-async-mode 1)))

(defun halidom/dired-reuse-buffer-hook ()
	(define-key dired-mode-map (kbd "^")
	  (lambda () (interactive) (find-alternate-file ".."))))

(setq dired-dwim-target t)

(use-package dired+
  :init
  (add-hook #'dired-mode-hook #'dired-hide-details-mode))

(when (symbolp 'org-file-apps)
  (add-to-list 'org-file-apps '(directory . emacs)))

(use-package neotree
  :after (projectile)
  :demand t
  :commands (neotree-project-dir)
  :bind
  (([f8] . neotree-project-dir))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-smart-open t)
  :config
  (progn
    ;; @source https://www.emacswiki.org/emacs/NeoTree
    (defun neotree-project-dir ()
      "Open NeoTree using the git root"
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                 (progn
                   (neotree-dir project-dir)
                   (neotree-find file-name)))
          (message "Project root not found."))))))

(use-package treemacs)

(use-package sr-speedbar)

(use-package codesearch)

(use-package avy
  :straight t)

(use-package ack
  :if (executable-find "ack")
  :straight t)

(use-package ag)

(use-package grep+)

(use-package dired-sidebar
    :commands (dired-sidebar-toggle-sidebar)
    :init
    (defun disable-icons ()
      (when (fboundp 'all-the-icons-dired-mode)
        (set (make-local-variable 'all-the-icons-dired-mode) nil)))
    :hook
    (dired-sidebar-mode . disable-icons)
    :bind
    ("C-c d" . dired-sidebar-toggle-sidebar))

(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;;; Begin Startup Group
;; startup buffer
;; Page break lines
(use-package page-break-lines
  :init
  (global-page-break-lines-mode))
;; Emacs dashboard
(use-package dashboard
  :demand t
  :init

  (defun goto-dashboard ()
    (interactive)
    (if (seq-contains (mapcar #'buffer-name (buffer-list)) "*dashboard*")
        (switch-to-buffer "*dashboard*")))

  (define-key goto-map "d" #'goto-dashboard)

  (if-not (global-page-break-lines-mode)
      (global-page-break-lines-mode))
  (dashboard-setup-startup-hook)

  :config
  (add-to-list 'dashboard-items '(agenda) t)

  (setq dashboard-banner-logo-title "Steak and Eggs in my Dojo."
        dashboard-items '(( agenda . 10)
                          ( projects . 5)
                          ( recents . 5)
                          ( bookmarks . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Fireplace
(use-package fireplace
  :straight t)
;; startup scratch and message
(setq initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t)
;; startup frame
;; Disable toolbar and scrollbar
(tool-bar-mode -1)

(scroll-bar-mode -1)
;; Disable menubar unless system is macOS.
(unless *is-mac*
  (menu-bar-mode -1))
;; Display Time
(display-time-mode 1)
;;; End Startup Group

(setq default-frame-alist
      '((ns-transparent-titlebar . t)
        (ns-appearance . dark)))

(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(use-package winner-mode
  :straight nil
  :init
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

(use-package popwin
  :config (popwin-mode 1))

(use-package poporg
      :bind (("C-c /" . poporg-dwim)))

(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale t))

;; Perspective
(use-package perspective)
;: Persp Projectile
(use-package persp-projectile
  :after (:all projectile counsel-projectile perspective)
  :bind ((:map projectile-mode-map
               ("s-S" . projectile-persp-switch-project))))

(use-package mark-forward-sexp
  :bind (("C-c o" . mark-forward-sexp)
         ("C-c O" . mark-backward-sexp)
         ("C-c i" . mark-inside-forward-sexp)
         ("C-c I" . mark-inside-backwad-sexp)))

(use-package help+
  :straight t)

(use-package help-macro+
  :straight t)

(use-package help-fns+
  :straight t)

(use-package help-mode+
  :straight t)

(use-package helpful
    :commands (helpful-at-point helpful-function helpful-command)
    :init

    (defhydra 'helpful (global-map "M-m h")
        "Helpful"
        ("d" helpful-at-point)
        ("f" helpful-function)
        ("c" helpful-command))

    (which-key-add-key-based-replacements
        "M-m h" "helpful"))

(use-package image+
  :if (display-graphic-p)
  :after (image)
  :config
  (eval-after-load 'image+
    `(when (require 'hydra nil t)
       (defhydra imagex-sticky-binding (global-map "C-x C-l")
         "Manipulating image"
         ("+" imagex-sticky-zoom-in "zoom in")
         ("-" imagex-sticky-zoom-out "zoom out")
         ("M" imagex-sticky-maximize "maximize")
         ("O" imagex-sticky-restore-original "restore orginal")
         ("S" imagex-sticky-save-image "save file")
         ("r" imagex-sticky-rotate-right "rotate right")
         ("l" imagex-sticky-rotate-left "rotate left")))))

(use-package info+
  :straight t)

(use-package font-lock+
  :straight t)

(use-package rainbow-mode)

(use-package col-highlight)

(use-package frame+)

(use-package frame-fns)

(use-package posframe)

(use-package thingatpt+)

(use-package misc-fns)

(use-package bookmark+)
(use-package bm)
(defadvice bookmark-write-file
      (after local-directory-bookmarks-to-zsh-advice activate)
   (local-directory-bookmarks-to-zsh))

(defun local-directory-bookmarks-to-zsh ()
      (interactive)
      (when (and (require 'tramp nil t)
                 (require 'bookmark nil t))
        (set-buffer (find-file-noselect "~/.zsh.bmk" t t))
        (delete-region (point-min) (point-max))
        (insert "# -*- mode:sh -*-\n")
        (let (collect-names)
          (mapc (lambda (item)
                  (let ((name (replace-regexp-in-string "-" "_" (car item)))
                        (file (cdr (assoc 'filename
                                           (if (cddr item) item (cadr item))))))
                    (when (and (not (tramp-tramp-file-p file))
                               (file-directory-p file))
                      (setq collect-names (cons (concat "~" name) collect-names))
                      (insert (format "%s=\"%s\"\n" name (expand-file-name file) name)))))
                bookmark-alist)
          (insert ": " (mapconcat 'identity collect-names " ") "\n"))
        (let ((backup-inhibited t)) (save-buffer))
        (kill-buffer (current-buffer))))

(use-package debbugs
  :straight (debbugs
             :type git
             :repo "https://git.savannah.gnu.org/git/emacs/elpa.git"
             :files ("packages/debbugs/*.el"
                     "packages/debbugs/Debbugs.wsdl")
             :local-repo "elpa"))

;;; Begin Theme Group
;; Theme interface
(defcustom halidom--theme-style 'dark
  "The default theme mode. Only symbols 'dark and 'light are considered."
  :type '(choice
          (const :tag "Use a dark theme by default." dark)
          (const :tag "Use a light theme by default" light)
          (const :tag "Ignore the value of this variable" nil)))

(defcustom halidom-term-theme 'wombat
  "The default term theme."
  :type 'symbol)

(defcustom halidom-prose-theme 'tsdh-light
   "The default theme for writing text."
   :type 'symbol)

(defcustom halidom-light-theme 'tsdh-light
  "The light theme."
  :type 'symbol)

(defcustom halidom-dark-theme 'tsdh-dark
  "The dark theme."
  :type 'symbol)

(defcustom halidom-theme 'tsdh-dark
  "The default theme to load."
  :type 'string)

(defcustom halidom-theme-colors nil
  "plist of colors for the current theme."
  :type '(plist :key-type symbol :value-type sexp))
;; Custom themes
(setq custom-safe-themes t)
(setq custom-theme-directory (emacs-dir "themes"))
;; Zenburn theme
(use-package zenburn-theme
  :straight t)
;; Poet Theme
(use-package poet-theme
  :init
  (setq halidom-light-theme 'poet))
;; Base16 Themes
(use-package base16-theme
    :defines halidom--base16-p base16-oceanicnext-colors
    :init
    (setq halidom-dark-theme 'base16-oceanicnext
          halidom-light-theme 'base16-default-light)
    (defun halidom--base16-p (&optional theme)
      (let ((theme (or theme halidom-theme "")))
        (string-prefix-p "base16-" (symbol-name theme))))
    (add-to-list 'custom-theme-load-path
                 (expand-file-name "straight/build/base16-theme"
                                   user-emacs-directory))
    :config
    (setq base16-distinct-fringe-background nil))

;; Load Theme
(setq-default custom-enabled-themes (list halidom-theme))

(defun remove-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (mapc #'disable-theme custom-enabled-themes))

(defvar halidom/load-theme-hook nil)


(defun halidom/load-theme ()
  (interactive)
  (let ((theme
         (cond ((eq halidom--theme-style 'dark)
                halidom-dark-theme)
               ((eq halidom--theme-style 'light)
                halidom-light-theme)
               (t halidom-term-theme))))
  (remove-themes)

  (if (display-graphic-p)
      (progn

        (load-theme theme)
        (setq halidom-theme theme)
        (if (halidom--base16-p theme)
            (setq halidom-theme-colors
                  (symbol-value
                   (intern
                    (eval
                     `(concat
                       ,(symbol-name (symbol-value 'halidom-theme)) "-colors")))))
          (setq halidom-theme-colors nil))
        (run-hooks 'halidom/load-theme-hook))


    (progn
      (load-theme halidom-term-theme)))))

(defun halidom/load-theme-with-frame (frame)
  (with-selected-frame frame
    (if-not (daemonp)
      (halidom/load-theme))))

(defun halidom--toggle-theme-style ()
  (interactive)
  (if (eq halidom--theme-style 'dark)
      (setq halidom--theme-style 'light)
    (setq halidom--theme-style 'dark)))

(defun halidom/toggle-theme ()
  (interactive)
  (halidom--toggle-theme-style)
  (halidom/load-theme))


(add-hook 'after-init-hook #'halidom/load-theme)
(add-hook 'after-make-frame-functions #'halidom/load-theme-with-frame)
;; Org Mode faces
(defun halidom/org-faces ()
  "Customize `org-mode' faces for base-16 themes."
  (let ((fg (face-foreground 'default nil 'default))
        (fg2 (or (plist-get halidom-theme-colors :base04)))
        (bg2 (or (plist-get halidom-theme-colors :base01)))
        (header-font "Sans Serif"))
    (if (and fg2 bg2)
        (progn
          (set-face-attribute 'org-level-1 nil
                             :family header-font
                             :height 1.4
                             :inherit 'outline-1)
          (set-face-attribute 'org-level-2 nil
                             :family header-font
                             :height 1.2
                             :inherit 'outline-2)
          (set-face-attribute 'org-level-3 nil
                              :family header-font
                              :height 1.0
                              :slant 'normal
                              :inherit 'outline-3)

          (set-face-attribute 'org-document-title nil
                              :height 1.5
                              :underline nil
                              :foreground fg
                              :inherit 'org-level-1)

          (set-face-attribute 'org-document-info-keyword nil
                              :foreground fg2
                              :slant 'italic
                              :inherit 'org-document-info-face)

          (set-face-attribute 'org-block nil
                              :foreground fg)

          (set-face-attribute 'org-block-begin-line nil
                              :foreground fg2
                              :background bg2
                              :inherit 'org-meta-line)

          (set-face-attribute 'org-block-end-line nil
                              :inherit 'org-block-begin-line)))))

(defun halidom--org-restart ()
  "Restart all open org-mode buffers."
  (let ((org-buffers (org-buffer-list)))
    (dolist (buf org-buffers)
      (with-current-buffer buf
        (org-mode-restart)))))

(add-hook 'halidom/load-theme-hook #'halidom/org-faces)
;; https://github.com/syl20bnr/spacemacs/pull/7667
(add-hook 'halidom/load-theme-hook #'halidom--org-restart)
;; Italicize keywords for modes
(defcustom halidom-italicize-keyword-modes '(emacs-lisp-mode js2-mode org-mode)
  "Major modes for which an italicized font lock keyword
face shall be used."
  :type '(symbol))

(defun halidom/italicize-keyword-fn ()
  (face-remap-add-relative 'font-lock-keyword-face
                           '(:slant italic)))

(defun halidom/italic-keyword-faces ()
  (cl-flet ((mode->hook (mode)
              (intern (concat (symbol-name mode) "-hook"))))
    (cl-loop
       for mode in halidom-italicize-keyword-modes
       for hooksym = (mode->hook mode)
       do
         (add-hook hooksym 'halidom/italicize-keyword-fn))))


(add-hook 'after-init-hook 'halidom/italic-keyword-faces)

;;; End Theme Group

(setq-default cursor-in-non-selected-windows nil
              x-stretch-cursor nil)

(modify-all-frames-parameters (list (cons 'cursor-type '(bar . 4))))

(blink-cursor-mode -1)

(use-package multiple-cursors
  :init
  (progn
    (global-unset-key (kbd "M-<down-mouse-1>")))
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))

(defvar font-list
  '()
  "An alist of fonts which can be interactively chosen
   using the `set-font' function.

   Each font entry is a cons cell composed of the font-family as a string and
   the associated font weight as an unquoted symbol.

   See `font-weight-table' for a list of available options.

   e.g (\"FONT-FAMILY\" . WEIGHT)"
  )

(when *is-mac*
  (let ((font-weight-xs '(("Andale Mono" . extralight)
                          ("Droid Sans Mono" . normal)
                          ("Fira Code" . light)
                          ("Hack" . normal)
                          ("Inconsolata" . medium)
                          ("Operator Mono" . extralight)
                          ("SF Mono" . normal)
                          ("Programma" . normal))))
  (mapcar (lambda (font-weight-x)
	    (add-to-list 'font-list font-weight-x))
	  font-weight-xs)))


(defun set-font (&optional font)
  "Set the custom FONT with completion"
  (interactive)
  (let* ((font (or font (assoc
			 (completing-read "Font: " font-list nil nil)
			 font-list)))
	 (family (car font))
	 (weight (cdr font)))
    (run-at-time "0.2 sec" nil
		 `(lambda () (when (not (eq (face-attribute 'default :family)
				       ,family))
			  (set-face-attribute 'default nil :family ,family
					      :weight (quote ,weight)))))))

(defcustom halidom-fonts
  '(("Operator Mono" . extralight)
    ("SF Mono" . normal)
    ("Lucida Sans Typewriter" . nil)
    ("Ubuntu Mono" . nil)
    ("Menlo" . nil)
    ("Inconsolota" . nil)
    ("Input Mono" . nil)
    ("Courier New" . nil))
  "The default font stack to use for setting the font
on startup and new frame."
  :type '(string))

(defcustom halidom-macos-default-font-size 13
  "The default font size in pixels."
  :type 'number)


(defun halidom/font-setup (&optional frame)
  (interactive)
  (let* ((font-family (or (car
                           (seq-intersection (mapcar #'car halidom-fonts) (font-family-list)))
                          (face-attribute 'fixed-pitch :family)))
         (weight (or (cdr (assoc font-family halidom-fonts)) 'normal)))
    (run-at-time "0.2 sec" nil
		             `(lambda () (when (not (eq (face-attribute 'default :family)
				                               ,font-family))
			                    (set-face-attribute 'default nil :family ,font-family
					                                    :weight (quote ,weight)
					                                    :height 120))))))



(add-hook 'after-init-hook 'halidom/font-setup)
(add-hook 'after-make-frame-functions 'halidom/font-setup)

;; Begin icons Group
;; All the Icons
;; All the icons package
(use-package all-the-icons
  :init
  (if (and *is-mac*
           (not
            (member "all-the-icons.ttf"
                    (directory-files (user-home "Library" "Fonts")))))
      (all-the-icons-install-fonts))

  :config
  (let ((props (cdr (assoc "\\.node" all-the-icons-icon-alist))))
    (add-to-list 'all-the-icons-icon-alist (cons "\\.mjs" props))))
;; All The Icons Dired
(use-package all-the-icons-dired
    :after (:all dired all-the-icons)
    :demand t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;; All the Icons Ivy

(use-package all-the-icons-ivy
  :after (:all ivy all-the-icons)
  :init
  (all-the-icons-ivy-setup))

;; Emojis
;; Unicode Fonts
(use-package unicode-fonts
  :init
  (unicode-fonts-setup))
;; Unicode emotions
(use-package unicode-emoticons)
;; Emojify Mode
(use-package emojify
  :init
  (add-to-hooks #'emojify-mode
                '(org-mode-hook
                  markdown-mode-hook
                  magit-status-mode-hook)))
;; Company Emoji

;; End icons Group

;;; Begin Modeline Group
;; Customization interface
(defcustom halidom-modeline nil
  "When non-nil, the modeline framework to use at startup.
Options are 'spaceline, 'sml, and 'powerline."
  :type '(choice
          (const :tag "Use the default modeline." nil)
          (const :tag "Use smart-mode-line" sml)
          (const :tag "Use powerline" powerline))
  :initialize 'custom-initialize-default)
;; Smart Mode Line
(use-package smart-mode-line)
;; Smart Mode Line setup

(defun ml-smline-setup ()
  "Setup the modeline for smart-mode-line."
  (require 'smart-mode-line)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup)
  (run-hooks 'sml-setup-hook))
;; Spaceline
(use-package spaceline)
;; Spaceline icons
(use-package spaceline-all-the-icons
  :after (:all all-the-icons spaceline))
;; setup spaceline
(defun ml-spaceline-setup ()
  "Setup the modeline for spaceline."
  (setq spaceline-all-the-icons-separator-type 'arrow)
  (require 'spaceline)
  (require 'spaceline-segments)
  (require 'spaceline-config)
  (require 'all-the-icons)
  (require 'spaceline-all-the-icons)
  (spaceline-all-the-icons--setup-neotree)
  (spaceline-all-the-icons-theme 'mu4e-alert 'org-pomodoro)
  (spaceline-toggle-all-the-icons-flycheck-status-off))
;; Powerline
(use-package powerline
    :straight t)
;; Powerline setup
(defun ml-powerline-setup ()
  "Setup the modeline for powerline."
  (require 'powerline)
  (powerline-default-theme))
;; Setup the modeline
(setq halidom-modeline 'spaceline)

(defun halidom/modeline-setup ()
  "Setup the modeline."
  (pcase halidom-modeline
    (`spaceline (ml-spaceline-setup))
    (`powerline (ml-powerline-setup))
    (`sml (ml-smline-setup))))

(defun halidom/modeline-setup-frame (frame)
  (with-selected-frame frame
    (when (daemonp)
      (if (display-graphic-p)
          (halidom/modeline-setup)
        (ml-smline-setup)))))

(add-hook 'after-init-hook 'halidom/modeline-setup)
(add-hook 'after-make-frame-functions 'halidom/modeline-setup-frame)

;; End Modeline Group

(setq compilation-scroll-output 'first-error)

(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1)
  :config
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively 101))

(setq redisplay-dont-pause t)

;;; Begin Completion Group
;; Begin Company
;; Company
(use-package company
  :commands global-company-mode
  :bind (("TAB" . company-indent-or-complete-common)
         ("C-c /" . company-files)
         ("M-SPC" . company-complete)
          (:map company-mode-map
                ("M-n" . company-select-next-or-abort)
                ("M-p" . company-select-previous-or-abort)))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
    (setq company-tooltip-limit 20
          company-tooltip-align-annotations t
          company-idle-delay .3
          company-begin-commands '(self-insert-command)))
;; Company quickhelp
(use-package company-quickhelp
  :after (company)
  :commands (company-quickhelp-manual-begin)
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1))
;; Company Statistics
(use-package company-statistics
  :after (company)
  :demand t
  :init
  (company-statistics-mode))
;; Company box
(use-package company-box
    :custom (company-box-enable-icon nil)
    :hook (company-mode . company-box-mode))
;; End Company
;; End Company
;; Begin Snippets
;; Yasnippet
(use-package yasnippet
    :bind
    (:map goto-map
          ("s" . goto-snippet-dir))

    :init
    (defvar snippet-directory (emacs-etc-dir "snippets")
      "Directory for yasnippets.")

    (defun goto-snippet-dir ()
      "Goto `snippet-directory'."
      (interactive)
      (let ((default-directory snippet-directory))
        (dired default-directory)))

    (yas-global-mode 1))

;; Code Library
(use-package code-library
  :after (org)
  :init
  (progn
    (setq code-library-directory (emacs-etc-dir "codelibrary"))
    (if-not (file-exists-p code-library-directory)
        (if (yes-or-no-p
             (format-message "Directory `%s' doesn't exist. Make directory?" code-library-directory))
            (mkdir code-library-directory))))
  :config
  (progn
    (append 'code-library-mode-file-alist
            '((latex-mode . "latex.org")
              (clojure-mode . "clojure.org")
              (makefile-mode . "makefile.org")
              (makefile-gmake-mode . "makefile.org")))
    (setq code-library-sync-to-gist t)))
;; End Snippets
;; Begin Minibuffer
;; Minibuffer Prompt Properties
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; Ivy Group
;; Ivy
(use-package ivy
    :init
    ;; Lazy Load Ivy
    ;; https://github.com/raxod502/el-patch#lazy-loading-packages
    ;; https://github.com/raxod502/radian/blob/develop/radian-emacs/radian-coxompletion.el#L25
    (el-patch-feature ivy)

    (el-patch-defvar ivy-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [remap switch-to-buffer]
          'ivy-switch-buffer)
        (define-key map [remap switch-to-buffer-other-window]
          'ivy-switch-buffer-other-window)
        map)
      "Keymap for `ivy-mode'.")

    (el-patch-define-minor-mode ivy-mode
        "Toggle Ivy mode on or off.
  Turn Ivy mode on if ARG is positive, off othherwise.

    Global bindings:
    \\{ivy-mode-map}

    Minibuffer bindings:
    \\{ivy-minibuffer-map}"

      :group 'ivy
      :global t
      :keymap ivy-mode-map
      :lighter " ivy"
      (if ivy-mode
          (progn
            (setq completing-read-function 'ivy-completing-read)
            (el-patch-splice 2
              (when ivy-do-completion-in-region
                (setq completion-in-region-function
                      'ivy-completion-in-region))))
        (setq completing-read-function 'completing-read-default)
        (setq completion-in-region-function 'completion--in-region)))
    (ivy-mode +1)
    (diminish 'ivy-mode)

  :bind (("C-c C-r" . ivy-resume)
         ("C-`" . ivy-avy))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)
  (ivy-sort-max-size 50000)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))

  :config
  (ivy-mode 1)

  :diminish ivy-mode)

;; Ivy Prescient
;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts. It is not published to MELPA, so we
;; must define a recipe here.
(use-package prescient
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `ivy-prescient' provides intelligent sorting and filtering
;; for candidates in Ivy menus.
(use-package ivy-prescient
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

;; Ivy Xref
(use-package ivy-xref
    :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
;; Counsel
;; Counsel
(use-package counsel
  :bind (("<f2> u" . counsel-unicode-char)
         ("<f1> l" . counsel-find-library)
         ("C-c l" . counsel-load-library)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         (:map minibuffer-local-map
               ("C-r" . counsel-minibuffer-history)))
  :init
  ;; Lazy-load `counsel'.
  (el-patch-defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (describe-function . counsel-describe-function)
                 (describe-variable . counsel-describe-variable)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)
                 (org-capture . counsel-org-capture)
                 (org-tag . counsel-org-tag)))
        (define-key map
            (vector 'remap (car binding)) (cdr binding))) map)
    "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements.")

   (el-patch-defcustom counsel-mode-override-describe-bindings nil
    "Whether to override `describe-bindings' when `counsel-mode' is active."
    :group 'ivy
    :type 'boolean)

  (el-patch-define-minor-mode counsel-mode
    "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements. "
    :group 'ivy
    :global t
    :keymap counsel-mode-map
    :lighter " counsel"
    (if counsel-mode
        (progn
          (when (and (fboundp 'advice-add)
                     counsel-mode-override-describe-bindings)
            (advice-add #'describe-bindings :override #'counsel-descbinds))
          (define-key minibuffer-local-map (kbd "C-r")
            'counsel-minibuffer-history))
      (when (fboundp 'advice-remove)
        (advice-remove #'describe-bindings #'counsel-descbinds))))

  ;; Use customized Ivy configurations for built-in Emacs commands.
  (counsel-mode +1)

  ;; Diminish the lazy-loaded version of `counsel-mode'.
  (diminish 'counsel-mode)

  :config
  (setq-default counsel-git-grep-cmd counsel-git-grep-cmd-default)
  (setq counsel-mode-override-describe-bindings t)

  :diminish counsel-mode)

;; Counsel Projectile
(use-package counsel-projectile
    :after (:all projectile counsel)
    :demand t
    :init
    (counsel-projectile-mode t))
;; Counsel Gtags
(use-package counsel-gtags
  :custom
	(counsel-gtags-ignore-case t)
  (counsel-gtags-auto-update t)

  :bind
  (:map counsel-gtags-mode-map
        ("M-m g d" . counsel-gtags-find-definition)
        ("M-m g r" . counsel-gtags-find-reference)
        ("M-m g s" . counsel-gtags-find-symbol)
        ("M-m g ," . counsel-gtags-go-backward))

  :config
  (which-key-add-key-based-replacements "M-m g"
      '("ggtags" . "Counsel Gtags"))

  :hook
  (c-mode-common . counsel-gtags-mode))
;; Counsel iTunes
(use-package counsel-itunes
  :if (executable-find "osascript")
  :straight
  (counsel-itunes
   :host github
   :repo "jchaffin/counsel-itunes")
  :demand t
  :after (:all counsel ivy)
  :init
   ;; make prefix command
  (define-prefix-command 'counsel-itunes-prefix-map)

   (let ((map counsel-itunes-prefix-map))
     (define-key map "-" '("Volume Down" . counsel-itunes-volume-down))
     (define-key map "+" '("Volume Up" .  counsel-itunes-volume-up))
     (define-key map "t" '("Tracklist" . counsel-itunes-tracklist))
     (define-key map "p" '("Playlist" . counsel-itunes-playlist))
     (define-key map "c" '("Now Playing" . counsel-itunes-current-track))
     (define-key map "P" '("Playlist" . counsel-itunes-playlist))
     (define-key map "n" '("Next" . counsel-itunes-next-track))
     (define-key map "b" '("Previous" . counsel-itunes-previous-track))
     (define-key map "s" '("Shuffle" . counsel-itunes-shuffle)))

   (define-key 'macos-prefix-map (kbd "i") 'counsel-itunes-prefix-map)
   (which-key-add-prefix-title "M-m m i" "iTunes"))

;; Counsel spotify
(use-package counsel-spotify
  :straight t)
;; Counsel Dash
(use-package counsel-dash
  :after (:all counsel)
  :if *is-mac*)
;; Counsel Tramp
(use-package counsel-tramp
    :bind
    ("C-c t" . counsel-tramp)
    :init
    (defun straight-recipes-installed (pkg)
      (f-dir? (f-join user-emacs-directory "straight" "build" (symbol-name pkg))))
    (defalias #'package-installed-p #'straight-recipes-installed)
    :after counsel)
;; Counsel Code Search
(use-package counsel-codesearch
    :requires codesearch)
;; Swiper

(use-package swiper
  :bind
  (("\C-s" . swiper)))

;; Smex
(use-package smex
  :after (ivy)
  :init
  (setq-default smex-history-length 32))
;; Omnibox

(use-package omnibox
    :commands omnibox-M-x
    :bind (:map omnibox-mode-map
                ("M-x" . omnibox-M-x))
    :init
    (let ((pkgdir "~/.local/share/icons-in-terminal"))
      (when (f-dir-p pkgdir)

      (add-to-list 'load-path pkgdir)))
      (when (locate-library "font-lock+")
        (require 'font-lock+)
        (require 'icons-in-terminal)))

;; End Minibuffer
;;; End Completion Group

(use-package sane-term
  :bind
  (("C-c M-RET t" . sane-term)
   ("C-c M-RET T" . sane-term-create))
  :config
  (when *is-mac*
    (setq sane-term-shell-command "/bin/zsh")))
(xterm-mouse-mode 1)
;; ZSH Shell Fix
(when *is-mac*
  (setq explicit-shell-file-name "/bin/sh"
	      shell-file-name "sh")
  (setenv "SHELL" shell-file-name))
;; Shell script style
(setq sh-basic-offset 2)
;; Shell Command to List
(defun shell-command-to-list (command)
    "Store stdout as an elisp list."
    (interactive "sCommand: " )

    (--> command
         (shell-command-to-string it)
         (split-string it)
         (if (interactive-p)
             (message "%s" it)
           it)))
(setq tramp-default-method "ssh")
(use-package anything-tramp
  :bind (("C-c s" . anything-tramp)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Escape ANSI color sequence in the compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun display-ansi-file ()
  (when (string= "ansi" (file-name-extension (buffer-file-name)))
    (display-ansi-colors)))

(add-hook 'find-file-hook #'display-ansi-file)

(when *is-windows*
  (progn
    (setq explicit-shell-file-name "C:/path/to/bash.exe"
          shell-file-name "bash"
          explicit-bash.exe-args '("--noediting" "--login" "-i"))
    (setenv "SHELL" shell-file-name)
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;; Begin Eshell
;; eshell bookmark
(use-package eshell-bookmark
    :config (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))
;; eshell z

(use-package eshell-z
    :init
    (defun eshell-z-hook ()
      (require 'eshell-z))
    (add-hook 'eshell-mode-hook #'eshell-z-hook))

;; eshell prompt extras
(use-package eshell-prompt-extras
    :custom
    (eshell-modify-global-environment t)
    :init
    (with-eval-after-load "esh-opt")
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-lambda))
;; End Eshell

(use-package ssh
    :init
  (defun halidom/ssh-mode-hook ()
    (setq ssh-directory-tracking-mode t)
    (shell-dirtrack-mode t)
    (setq dirtrackp nil))
    :hook (ssh-mode . halidom/ssh-mode-hook))

(use-package scp)

;; Browser
(define-prefix-command 'web-browse-prefix-map)
(define-key 'halidom-prefix-map (kbd "w") 'web-browse-prefix-map)
(which-key-add-prefix-title "M-m w" "Web Browser")

(use-package xwidget
    :straight nil
    :bind (:map xwidget-webkit-mode-map
                ("\C-s" . isearch-forward))
    :init
    (el-patch-feature xwidget)
    (with-eval-after-load 'xwidget
      (el-patch-defun xwidget-webkit-new-session (url)
        "Create a new webkit session buffer with URL."
        (let*
            ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
             xw)
          (setq xwidget-webkit-last-session-buffer
                ((el-patch-swap switch-to-buffer switch-to-buffer-other-window)
                 (get-buffer-create bufname)))
          ;; The xwidget id is stored in a text property, so we need to have
          ;; at least character in this buffer.
          ;; Insert invisible url, good default for next `g' to browse url.
          (insert url)
          (put-text-property 1 (+ 1 (length url)) 'invisible t)
          (setq xw (xwidget-insert 1 'webkit bufname
                                   (xwidget-window-inside-pixel-width (selected-window))
                                   (xwidget-window-inside-pixel-height (selected-window))))
          (xwidget-put xw 'callback 'xwidget-webkit-callback)
          (xwidget-webkit-mode)
          (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))))

    (when (require 'xwidget)
      (defvar xwidget-webkit-bookmark-jump-new-session) ;; xwidget.el
      (defvar xwidget-webkit-last-session-buffer) ;; xwidget.el

      (require 'bookmark)

      (add-hook 'pre-command-hook
                (lambda ()
                  (if (eq this-command #'bookmark-bmenu-list)
                      (if-not (eq major-mode 'xwidget-webkit-mode)
                          (setq xwidget-webkit-bookmark-jump-new-session t)
                        (setq xwidget-webkit-bookmark-jump-new-session nil)
                        (setq xwidget-webkit-last-session-buffer
                              (current-buffer))))))

      (defun xwidget-webkit-open-file (&optional file)
        "Render FILE using xwidget-webkit"
        (interactive "fFile: ")
        (xwidget-webkit-browse-url
         (concat "file://"
                 (and (memq system-type '(windows-nt ms-dos)) "/")
                 (expand-file-name (or file (buffer-file-name))))))))
(use-package xwidgete
  :after (xwidget))

(use-package browse-url
    :custom (browse-url-chromium-program
             (if *is-mac*
                 "/Applications/Chromium.app/Contents/MacOS/Chromium"
               "chromium"))
  :config
  (progn
    (when (not (display-graphic-p))
      (setq browse-url-browser-function 'eww-browse-url))))

(use-package osx-browse
  :if *is-mac*
  :defines (osx-browse-mode osx-browse-mode-map)
  :demand t
  :init
  (progn
    (osx-browse-mode 1)))
(defun google-search-query-at-point (&optional edit-query)
  "Search for the expression at point in the default web browser.
If the optional prefix EDIT-QUERY is specified,
the user will be prompted to edit the search string first."
  (interactive "P")
  (let* ((search-prefix "https://google.com/search?q=")
         (thing (if (thing-at-point 'url)
                    (thing-at-point-url-at-point)
                  (concat
                   search-prefix
                   (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (thing-at-point 'word)))))
         (search (if (or edit-query
                         (and (>= (length thing) (length search-prefix))
                              (string= (substring thing (length search-prefix)) "")))
                     (concat search-prefix
                             (read-from-minibuffer "Search Query: "))
                   thing)))
    (browse-url search)))
(use-package search-web
  :init
  (define-key 'web-browse-prefix-map "s" 'search-web))
(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (if (and (> emacs-major-version 26)
                           (display-graphic-p))
                      (xwidget-webkit-browse-url url t)
                    (eww-browse-url url t)))))))
    ad-do-it))
(use-package browse-at-remote
  :init
  (progn
    (if *is-mac*
        (when (fboundp 'osx-browse-url-chrome)
          (setq browse-url-browser-function 'osx-browse-url-chrome)))))

  (use-package engine-mode
    :init
    (engine-mode t)

    :config
    (setq halidom-web-engine-prefix "M-m w e")

    (when engine/keybinding-prefix
      (define-key engine-mode-map (kbd "C-x /") nil)
      (define-key 'web-browse-prefix-map "e" engine-mode-prefixed-map))

    (defun engine-add-which-key-replacement (engine prefix &optional keybinding)
      (let* ((keyseq (concat prefix " " keybinding))
	     (engine-string (upcase-initials (symbol-name engine)))
	     (replacement (list engine-string
				(concat "Search " engine-string))))
	(which-key-add-key-based-replacements keyseq engine-string)))

    (cl-defun engine-key-replacement (orig-function &rest args)
      (let ((keybinding (plist-get (cddr args) :keybinding))
	    (engine (car args))
	    (prefix "M-m w e"))
	(engine-add-which-key-replacement engine prefix keybinding)
	(apply orig-function args)))

    (advice-add 'defengine :around #'engine-key-replacement)

    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "h"
      :docstring "Search Github")

    (defengine worldcat
      "https://ucla.worldcat.org/search?q=%s"
      :keybinding "W"
      :docstring "Search Worldcat")

    (defengine proquest
      "https://search.proquest.com"
      :keybinding "p"
      :docstring "Search Proquest")

    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g"
      :docstring "Search Google")

    (defengine cassi
      "http://cassi.cas.org/search.jsp"
      :browser 'xwidget-webkit-browse-url)

    (defengine google-bookmarks
      "chrome://bookmarks/?q=%s"
      :keybinding "Gb"
      :docstring "Search bookmarks in Google Chrome")

    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
	:keybinding "Gi"
	:docstring "Search google-images")

    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :keybinding "Gm"
      :docstring "Mappin' it up.")

    (defengine project-gutenberg
      "http://www.gutenberg.org/ebooks/search/?query=%s"
      :docstring "Read good")

    (defengine rfcs
      "http://pretty-rfc.herokuapp.com/search?q=%s")

    (defengine stack-overflow
      "https://stackoverflow.com/search?q=%s"
      :keybinding "s"
      :docstring "Search StackOverflow.")

    (defengine twitter
      "https://twitter.com/search?q=%s")

    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w"
      :docstring "Search wikipedia.")

    (defengine wiktionary
      "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

    (defengine wolfram-alpha
      "http://www.wolframalpha.com/input/?i=%s"
      :keybinding "a"
      :docstring "Search Wolfram Alfa")

    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
	:keybinding "y"
	:docstring "Search Youtube")

    (defengine ctan
      "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s"
      :docstring "Search the Comprehensive TeX Archive Network (ctan.org)")

   (cl-flet* ((ks (&rest k)
		  (let ((p halidom-web-engine-prefix))
		    (if k (join (push p k) " ") p)))
	      (wkr (rep &rest k)
		   (let ((kk (apply #'ks k)))
		     (which-key-add-key-based-replacements kk rep))))
     (wkr "Search Engine")
     (wkr "Google Engines" "G")
     (wkr "Google Images" "G" "i")
     (wkr "Google Bookmarks" "G" "b")))

;; Email
;; Offline imap
(use-package offlineimap)
;; Mu
;; Mu4e
(use-package mu4e
    :straight nil
    :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
    :init
    (require 'mu4e)

    :config
    ;; use mu4e for email in emacs
    (setq mail-user-agent 'mu4e-user-agent)

    (setq mu4e-maildir "~/.mail")

    ;; Update with offlineimap
    (setq mu4e-get-mail-command "offlineimap -o -q")

    ;; Let GMAIL/IMAP take care of sent messages
    (setq mu4e-sent-messages-behavior 'delete)

    (require 'smtpmail)

     ;; smtp settings
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    ;; Kill message buffers on send
    (setq message-kill-buffer-on-exit t)

    (require 'mu4e-contrib)

    ;; mu4e View
    (setq mu4e-attachment-dir "~/Downloads"
          mu4e-view-show-images t
          mu4e-show-addresses t
          mu4e-view-prefer-html t
          mu4e-view-show-images t)


    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (setq mu4e-completing-read-function 'ivy-completing-read)

    ;; Mu4e Contexts
    (setq mu4e-context-policy 'pick-first
          mu4e-compose-context-policy 'ask-if-none)

    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name "personal"
              :enter-func
              (lambda () (mu4e-message "Using Personal gmail account.k"))
              :leave-func
              (lambda () (mu4e-message "leaving Personal gmail account."))
              :match-func
              (lambda (msg)
                (when msg
                  (string-match-p "^/personal")
                  (mu4e-message-field msg :maildir)))
              :vars
              '((user-mail-address  . "jchaffin57@gmail.com")
                (user-full-name     . "Jacob Chaffin")
                (mail-reply-to      . "jchaffin57@gmail.com")
                (smtpmail-smtp-user . "jchaffin57@gmail.com")
                (mu4e-sent-folder   . "/personal/sent")
                (mu4e-drafts-folder . "/personal/drafts")
                (mu4e-refile-folder . "/personal/archive")
                (mu4e-trash-folder  . "/personal/trash")
                (mu4e-compose-signature .
                 (concat
                  "Jacob Chaffin\n"
                  "jchaffin57@gmail.com\n"
                  "jchaffin@ucla.edu"))))

             ,(make-mu4e-context
               :name "school"
               :enter-func
               (lambda () (mu4e-message "Entering UCLA Gmail account"))
               :leave-func
               (lambda () (mu4e-message "Leaving UCLA Gmail account."))
               :match-func
               (lambda (msg)
                 (when msg
                   (string-match-p "^/school" 
                   (mu4e-message-field msg :maildir))))
               :vars '((user-mail-address  . "jchaffin@ucla.edu")
                       (user-full-name     . "Jacob Chaffin")
                       (mail-reply-to      . "jchaffin@ucla.edu" )
                       (smtpmail-smtp-user . "jchaffin@g.ucla.edu")
                       (mu4e-drafts-folder . "/school/drafts")
                       (mu4e-sent-folder   . "/school/sent")
                       (mu4e-refile-folder . "/school/archive")
                       (mu4e-trash-folder  . "/school/trash")
                       (mu4e-compose-signature .
                        (concat
                         "Jacob Chaffin\n"
                         "UCLA 2019\n"
                         "Linguistics and Computer Science\n"
                         "jchaffin@ucla.edu"))))))

    ;; From [[https://www.djcbsoftware.nl/code/mu/mu4e/Some-context-tricks.html#Some-context-tricks][9.5 - Some Context Tricks]] 
    ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
    ;; `user-mail-address' values for all contexts. If you have other mail
    ;; addresses as well, you'll need to add those manually.
    (setq mu4e-user-mail-address-list
          (delq nil
                (mapcar (lambda (context)
                          (when (mu4e-context-vars context)
                            (cdr
                             (assq
                              'user-mail-address
                              (mu4e-context-vars context)))))
                        mu4e-contexts)))
    

    (require 'gnus-dired)

    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let ((buffers))
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffers) buffers))))
        (nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))
;; Mu4e Alert
(use-package mu4e-alert
    :init
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))
;; Mu4e Maildirs extension
(use-package mu4e-maildirs-extension
    :after mu4e
    :init
    (mu4e-maildirs-extension-load))
;; Cloud
(use-package org-onenote
  :straight t)

(use-package simple-httpd
    :straight (simple-httpd
               :type git
               :host github
               :repo "skeeto/emacs-web-server"
               :local-repo "simple-httpd"))

(use-package websocket)

(use-package uuidgen)

(use-package web-server)

(use-package browse-at-remote
  :init
  (progn
    (if *is-mac*
        (when (fboundp 'osx-browse-url-chrome)
          (setq browse-url-browser-function 'osx-browse-url-chrome)))))

(defvar browse-url-browser-alist
      '(("chrome" . browse-url-chrome)
        ("firefox" . browse-url-firefox)
        ("eww" . eww-browse-url)
        ("xwidget-webkit" . xwidget-webkit-browse-url)))

(if *is-mac*
    (progn
      (setf (cdr (assoc "chrome" browse-url-browser-alist))
            #'osx-browse-url-chrome)
      (setf (cdr (assoc "firefox" browse-url-browser-alist))
            #'osx-browse-url-firefox)
      (add-to-list 'browse-url-browser-alist
                   '("safari" . osx-browse-url-safari))))

(defun set-browser-function (browser)
  "Interactively set the browser used by `browse-url'. "
  (interactive
   (list (completing-read "Browser: " browse-url-browser-alist)))
  (when-let ((browser-function (cdr (assoc browser browse-url-browser-alist))))
    (setq browse-url-browser-function browser-function)))


(define-key 'web-browse-prefix-map "B" 'set-browser-function)

(which-key-add-key-based-replacements "M-m w B"
    '("Default Browser" . "Set the default web browser"))

(cl-defun make-browser-function (browser)
  `(defun ,(intern (concat "set-browser-function-" browser)) ()
     (interactive)
     (set-browser-function ,browser)))

(defmacro make-browser-functions (browsers)
  `(progn ,@(mapcar 'make-browser-function browsers)))


(eval `(make-browser-functions ,(mapcar 'car browse-url-browser-alist)))

(define-prefix-command 'web-browse-browser-prefix-map)

(let ((map web-browse-browser-prefix-map))
  (define-key map "c" 'set-browser-function-chrome)
  (define-key map "e" 'set-browser-function-eww)
  (define-key map "f" 'set-browser-function-firefox)
  (define-key map "s" 'set-browser-function-safari)
  (define-key map "x" 'set-browser-function-xwidget-webkit))

(define-key 'web-browse-prefix-map "b" 'web-browse-browser-prefix-map)

(which-key-add-prefix-title "M-m w b" "Configure")

(use-package offlineimap)

;; Spellcheck
;; Flyspell
  (use-package flyspell
    :init
    (with-eval-after-load 'org
      (add-hook 'org-mode-hook 'flyspell-mode))
    :config
    (progn
      (when (executable-find "hunspell")
        (setq-default ispell-program-name "hunspell")
        (setq-default ispell-dictionary "en_US")
        (setq ispell-really-hunspell t))))
(use-package flyspell-correct-ivy
  :after (:all flyspell ivy)
  :demand t
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))
;; Langtool
(use-package langtool
  :if *is-mac*
  :after (flyspell)
  :demand t
  :config
  (progn
    (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-commandline.jar"
          langtool-mother-tongue "en"
          langtool-disabled-rules '("WHITESPACE_RULE"))))
;; Academic
(use-package academic-phrases
  :straight t)
;; Proselint
(with-eval-after-load 'flycheck
  (flycheck-define-checker proselint
                           "A linter for prose."
                           :command ("proselint" source-inplace)
                           :error-patterns
                           ((warning line-start (file-name) ":" line ":" column ": "
	                                   (id (one-or-more (not (any " "))))
	                                   (message) line-end))
                           :modes (text-mode org-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;; Completion
;; Company Dictionary
(use-package company-dict
  :after (company)
  :demand t
  :init
  (add-to-list 'company-backends 'company-dict)
  :config
  (setq company-dict-enable-fuzzy t
        company-dict-enable-yasnippet t))
;; Dictionary.el
(use-package dictionary
  :commands (dictionary-lookup-definition)
  :init
  (define-prefix-command 'dictionary-keymap))
;; Notes
;; Deft
(use-package deft
  :if *is-mac*
  :bind ("C-x C-n" . deft)
  :config
  (progn
    (setq deft-extensions '("org")
          deft-directory "~/Dropbox/org/notes/"
          deft-use-filename-as-title t
          deft-default-extension "org")))
;; Lorem Ipsum
(use-package lorem-ipsum
  :straight t)
;; Readview
;; Justify Kp
(use-package justify-kp
  :straight (:host github
                   :repo "Fuco1/justify-kp"))
;; Nov
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :config
  (progn

    (require 'justify-kp)


    (defun nov-setup ()
      (face-remap-add-relative 'variable-pitch
                               :family "Bookmania")
      (visual-line-mode 1)
      (visual-fill-column-mode 1)

      (setq nov-text-width most-positive-fixnum
            visual-fill-column-center-text t))

    (defun my-nov-window-configuration-change-hook ()
      (my-nov-post-html-render-hook)
      (remove-hook 'window-configuration-change-hook
                   'my-nov-window-configuration-change-hook t))

    (defun my-nov-post-html-render-hook ()
      (if (get-buffer-window)
          (let ((max-width (pj-line-width))
                buffer-read-only)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (not (looking-at "^[[:space:]]*$"))
                  (goto-char (line-end-position))
                  (when (> (shr-pixel-column) max-width)
                    (goto-char (line-beginning-position))
                    (pj-justify)))
                (forward-line 1))))
        (add-hook 'window-configuration-change-hook
                  'my-nov-window-configuration-change-hook
                  nil t)))


    (add-hook 'nov-mode-hook 'nov-setup)
    (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)))
;; Visual Fill Column
(use-package visual-fill-column
  :commands (visual-fill-column-mode)
  :config
    (progn
      (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
      (setq split-window-preferred-function
            #'visual-fill-column-split-window-sensibly)
      (setq visual-fill-column-width
            ;; take Emacs 26 line numbers into account
            (+ (if (boundp 'display-line-numbers) 6 0)
               fill-column))

      ;;;###autoload
      (defvar visual-fill-column-use-visual-line nil
        "Enable `visual-line-mode' when `visual-fill-column-mode' is non-nil.")

      (if-not visual-fill-column-use-visual-line
          (add-hook 'visual-fill-column-mode-hook #'visual-line-mode))))

;; Fill Column Indicator
(use-package fill-column-indicator
  :init
  (setq fci-rule-use-dashes nil))
;; Adaptive Wrap
(use-package adaptive-wrap
  :straight t)

;; Centered Cursor
(use-package centered-cursor-mode)
;; Olivetti Mode

(use-package olivetti
  :config
  (defun halidom/olivetti-mode-setup ()
    (auto-fill-mode -1)
    (when (fboundp 'centered-cursor-mode)
      (centered-cursor-mode)))

  (add-hook 'olivetti-mode-hook #'halidom/olvetti-mode-setup))

;; Writeroom Mode
(use-package writeroom-mode
  :config
  (defun halidom/writeroom-mode-hook ()
    (org-toggle-variable-pitch)
    (auto-fill-mode -1))
  (add-hook 'writeroom-mode #'halidom/writeroom-mode-hook))
;: Org Variable Pitch Face Font
(defcustom halidom-org-fixed-pitch-faces
  '(org-table
    org-code
    org-special-keyword
    org-verbatim
    org-meta-line
    org-block
    org-block-begin-line
    org-block-end-line
    org-done
    org-document-info-keyword)
  "Faces to keep fixed-width when using ‘org-variable-pitch-minor-mode’."
  :type '(list symbol))

(defvar org-fixed-pitch-font
  (face-attribute 'fixed-pitch :family))

(defvar org-variable-pitch-font
  (face-attribute 'variable-pitch :family))

(setq org-fixed-pitch-font "Monospace")


(defun toggle-variable-pitch-font ()
  (interactive)
  (let ((spec `(default variable-pitch :family ,org-variable-pitch-font)))
    (set (make-local-variable 'face-remapping-alist)
         (if (member spec face-remapping-alist)
             (delete spec face-remapping-alist)
           (cons spec face-remapping-alist)))))

(defun org-toggle-variable-pitch ()
  "Toggle use of face `variable-pitch'.
This works by frobbing `face-remapping-alist'."
  (interactive)
  (if-not face-remapping-alist
      (progn
        (face-remap-add-relative 'variable-pitch
                                 :family org-variable-pitch-font)
        (face-remap-add-relative 'default 'variable-pitch)
        (->> halidom-org-fixed-pitch-faces
            (mapcar (lambda (x) (list x :family org-fixed-pitch-font)))
            (mapcar (lambda (x) (apply #'face-remap-add-relative x)))))
    (setq face-remapping-alist nil)))


(defvar-local org-use-variable-pitch nil)
;; (when (local-variable-if-set-p 'org-use-variable-pitch)
;;   (add-hook 'org-mode-hook #'org-toggle-variable-pitch))

(use-package artist-mode
  :straight nil
  :bind ((:map artist-mode-map
               ("C-c C-a p" . artist-select-op-pen-line))))

(use-package graphviz-dot-mode
  :if (executable-find "dot")
  :mode "\\.dot\\'"
  :defines graphiz-dot-program-set
  :bind ((:map graphviz-dot-mode-map
               ("M-s g" . graphviz-dot-program-set)))
  :config
  (defun graphviz-dot-program-set (&optional program)
    "Set the process for `graphviz-dot-dot-program' interactively."
    (interactive)
    (let ((dot-program (or program (completing-read "Process: " graphviz-dot-layout-programs))))
      (setq graphviz-dot-dot-program dot-program)))

  (defun halidom/graphviz-dot-mode-setup ()
    "Setup graphviz dot mode."
    (define-key graphviz-dot-mode-map "{" nil)
    (define-key graphviz-dot-mode-map "}" nil)
    (add-to-list 'org-babel-load-languages '(dot . t))
    (setq org-src-lang-modes
          (append '(("dot" . graphviz-dot))
                  (delete '("dot" . fundamental) org-src-lang-modes))))

  (with-eval-after-load 'graphviz-dot-mode
    (halidom/graphviz-dot-mode-setup)))

(use-package thesaurus
  :config
  (progn
    ;; `thesaurus-bhl-api-key' is set in secrets
    (setq thesaurus-prompt-mechanism 'counsel-imenu
          url-proxy-services nil)))

;; LSP Mode
(use-package lsp-mode
  :custom
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-project-blacklist '("^/ssh:" "node_modules"))
  :config

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (defun lsp/set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))

  (add-hook 'lsp-before-open-hook 'lsp/set-projectile-root)

  (defun lsp/save-file-immediately ()
    (when lsp--cur-workspace
      (save-buffer)))

  (add-hook 'lsp-after-open-hook 'lsp/save-file-immediately))
;; LSP UI Mode
(use-package lsp-ui
    :bind
    (:map lsp-ui-mode-map
     ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
     ([remap xref-find-references]  . lsp-ui-peek-find-references))

    :custom
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-max-width 100)
    (lsp-ui-sideline-show-symbol nil)

    :hook (lsp-mode . lsp-ui-mode))

;; Company LSP
(use-package company-lsp
    :after company
    :demand t
    :init
    (push 'company-lsp company-backends)

    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil))

  (use-package flycheck
      :custom
      (flycheck-global-modes nil)
      (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
      (flycheck-emacs-lisp-load-path 'inherit)

      :init

      (defun halidom/flycheck-enable (mode)
        "Use flycheck in MODE."
        (push mode flycheck-global-modes))

      (defun halidom/flycheck-enable-hook ()
        "Enable Flycheck as a hook."
        (when (fboundp 'flycheck-mode)
          (flycheck-mode +1)))

      (defun halidom/toggle-flycheck-error-list ()
        "Toggle flycheck's error list window.
  If the error list is visible hide the window. Else display the buffer."
        (interactive)
        (-if-let (window (flycheck-get-error-list-window))
            (quit-window nil window)
          (flycheck-list-errors)))

      (defun halidom/goto-flycheck-error-list ()
        "Open and go to the error list buffer."
        (interactive)
        (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
          (flycheck-list-errors)
          (switch-to-buffer-other-window flycheck-error-list-buffer))))

;; Disable tabs
(setq-default tab-width 2
              indent-tabs-mode nil)

; Line Numbering
(when (>= emacs-major-version 26)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; Overlay Highlight
(use-package ov-highlight
  :straight (ov-highlight
             :host github
             :repo "jkitchin/ov-highlight")
  :after ov)
;; Highlight Sexp
(use-package highlight-sexp
  :straight t)
;; Highlight Todos
(use-package hl-todo
  :commands (hl-todo-mode)
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))
;; Highlight Symbol
(use-package highlight-symbol
  :straight t)
;; Prettify Symbols
(when (display-graphic-p)
  (add-hook 'prog-mode-hook 'prettify-symbols-mode))
;; Pretty Mode
(use-package pretty-mode
    :hook
  (emacs-lisp-mode . turn-on-pretty-mode)
  (LaTeX-mode . turn-on-pretty-mode))

(use-package origami
  :requires (dash s)
  :init
  (global-origami-mode nil))

;; Editorconfig
(use-package editorconfig
  :if (executable-find "editorconfig")
  :init (editorconfig-mode 1))

(use-package google-c-style
  :straight
  (google-c-style
   :host github
   :repo "google/styleguide"
   :branch "gh-pages")
  :init
  (defun halidom/google-c-style ()
    (google-set-c-style)
    (google-make-newline-indent))
  :hook
  (c++-mode . halidom/google-c-style)
  (java-mode . halidom/google-c-style))

(use-package format-all)

(use-package paredit
  :diminish paredit-mode
  :config
  (progn
    (use-package eldoc
      :config
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round))

    (autoload 'enable-paredit-mode "paredit"
      "Turn on pseudo-structural editing of Lisp code." t)))

(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)

    (defun sp-wrap-inline-math ()
      "Wrap marked region as ordinary LaTeX inline math mode."
      (interactive)
      (sp-wrap-with-pair "$"))

    (defun disable-smartparens ()
      "Disable smartparens when `paredit-mode' is enabled."
      (smartparens-mode -1))

    (when (fboundp 'paredit-mode)
      (add-hook 'paredit-mode-hook #'disable-smartparens))))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ggtags
  :if (and (getenv "GTAGSLABEL") (executable-find "global"))
  :custom (ggtags-highlight-tag nil))

(defcustom halidom-vcs-svn-list '(git)
  "List of VCS-SVNs for which libraries and tooling should be installed.
  Options are `git' and/or `hg' (mercurial) as symbols in a list, else nil."
  :type '(list symbol))
;; Mercurial
(when (or (member 'hg halidom-vcs-svn-list)
         (executable-find "hg"))
  ;; hg-monky
  (use-package monky
    :custom
    (monky-process-type 'cmdserver))
  ;; hg-ahg
  (use-package ahg)
)

;; Git
(when (or (member 'git halidom-vcs-svn-list)
         (executable-find "git"))
  ;; Magit
  ;; Magit Kill Buffers
  (defun magit-done (&optional no-prompt)
    "Kill magit buffers upon completion of various git processe(s).
  If called with the interactive prefix argument NO-PROMPT, then
  yunmodified magit buffers will be killed without confirming."
    (interactive "P")
    (let* ((buffer-names (buffer-list-names))
  	 (magit-regexp-string "^\\*magit")
  	 (magit-buffer-names (seq-filter (lambda (b) (string-match magit-regexp-string b)) buffer-names))
  	 (magit-buffers (mapcar (lambda (b) (get-buffer b)) magit-buffer-names)))
      (cond (no-prompt
  	   (mapcar
  	    (lambda (b)
  	      (if (> (window-count-unique) 1)
  		  (progn
  		    (let ((w (get-buffer-window)))
  		      (kill-buffer b)
  		      (delete-window w)))
  		(kill-buffer b)))
  	    magit-buffers))
  	  (magit-buffers
  	   (kill-some-buffers magit-buffers))
  	  (t
  	   (message "No magit buffer(s) to kill" )))))
  (defun magit-done-no-prompt ()
    "Close magit buffers without prompting."
      (interactive)
      (magit-done 1))
  ;; Magit mode
  (use-package magit
    :defines (magit-mode-hook)
    :bind
    (("C-c v v" . magit-status)
     ("C-c v m" . magit-merge)
     ("C-c v b" . magit-blame)
     ("C-c v C" . magit-clone)
     ("C-c v i" . magit-init)
     ("C-c v l" . magit-log-buffer-file)
     ("C-c v c" . magit-checkout)
     ("C-c v f" . magit-stage-file)
     ("C-c v p" . magit-pull)
     ("C-c v P" . magit-push)
     ("C-c v S" . magit-stash))
    :config
    (progn
      (setq magit-save-repository-buffers 'dontask)))
  
  ;; Magithub
  (use-package magithub
    :after (magit)
    :commands magithub-dispatch-popup
    :bind (:map magit-status-mode-map
  	      ("@" . magithub-dispatch-popup))
    :config
    (progn
      (magithub-feature-autoinject t)))
  ;; Orgit
  (use-package orgit
    :straight t)
  ;; Topgit
  (use-package magit-topgit
    :init
    (progn
      (add-hook 'magit-mode-hook 'turn-on-magit-topgit))
    :demand t
    :after (magit))
  ;; Stgit
  (use-package magit-stgit
    :init
    (progn
      (add-hook 'magit-mode-hook 'magit-stgit-mode))
    :demand t
    :after (magit))
  ;; Gist
  (use-package gist
    :bind
    (("C-c C-g l" . gist-list)
     ("C-c C-g r" . gist-region)
     ("C-c C-g b" . gist-buffer)
     ("C-c C-g p" . gist-buffer-private)
     ("C-c C-g B" . gist-region-or-buffer)
     ("C-c C-g P" . gist-region-or-buffer-private)))
  ;; Git Timemachine
  (use-package git-timemachine
    :bind
    ("C-c v t" . git-timemachine-toggle)
    :config
    (setq git-timemachine-abbreviation-length 7))
  ;; Git Messenger
  (use-package git-messenger
    :bind
    (("C-c C-v m" . git-messenger:popup-message)))
  ;; Git Modes
  (use-package git-modes
    :straight t
    :mode (".projectile\\'" . gitignore-mode))
)
;; Diff

(use-package ediff
:custom
(ediff-diff-options "-w"))

(defun magit-done (&optional no-prompt)
  "Kill magit buffers upon completion of various git processe(s).
If called with the interactive prefix argument NO-PROMPT, then
yunmodified magit buffers will be killed without confirming."
  (interactive "P")
  (let* ((buffer-names (buffer-list-names))
	 (magit-regexp-string "^\\*magit")
	 (magit-buffer-names (seq-filter (lambda (b) (string-match magit-regexp-string b)) buffer-names))
	 (magit-buffers (mapcar (lambda (b) (get-buffer b)) magit-buffer-names)))
    (cond (no-prompt
	   (mapcar
	    (lambda (b)
	      (if (> (window-count-unique) 1)
		  (progn
		    (let ((w (get-buffer-window)))
		      (kill-buffer b)
		      (delete-window w)))
		(kill-buffer b)))
	    magit-buffers))
	  (magit-buffers
	   (kill-some-buffers magit-buffers))
	  (t
	   (message "No magit buffer(s) to kill" )))))
(defun magit-done-no-prompt ()
  "Close magit buffers without prompting."
    (interactive)
    (magit-done 1))

(defun magit-done-no-prompt ()
  "Close magit buffers without prompting."
    (interactive)
    (magit-done 1))

(use-package git-modes
  :straight t
  :mode (".projectile\\'" . gitignore-mode))

;; Docker
;; Docker

(use-package docker
    :bind
  ("M-m d" . docker))
;; Dockerfile Mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
;; Docker Compose Mode
(use-package docker-compose-mode
    :mode ("docker-compose.yml\\'" . docker-compose-mode))

;; Docker Tramp
(use-package docker-tramp
  :straight t)
;; AWS
(use-package aws
  :config
  (progn
    (autoload 'ec2-desribe-instances "aws")
    (autoload 'ec2-describe-volumes "aws")
    (autoload 'ec2-describe-snapshots "aws")
    (autoload 'ec2-describe-group "aws")
    (autoload 'ec2-get-console "aws")))

  (use-package wakatime-mode
    :if (executable-find "wakatime")
    :init
    (add-hook 'prog-mode-hook 'wakatime-mode)
    :config
    (progn
      (if *is-mac*
          (setq wakatime-cli-path
                (f-expand "~/.local/lib/python3.6/site-packages/wakatime/cli.py")
                wakatime-python-bin
                (f-expand "~/.pyenv/shims/python")))

      ;; from spacemacs wakatime layer
      (defun wakatime-dashboard ()
        (interactive)
        (browse-url "https://wakatime.com/dashboard"))))

(use-package realgud
  :straight t)

(use-package logview)

(use-package lognav-mode)

(use-package floobits
  :if (file-exists-p (user-home ".floorc.json")))

(defcustom halidom-proglang-enabled-list nil
  "List of languages for which straight should install respective tooling,
   syntax-highlighting, and peripherals."
  :type '(list symbol))


(defun halidom/proglang-enabled-p (lang)
  "Returns non-nil if LANG is an enabled language spec."
  (member lang halidom-proglang-enabled-list))

(defun halidom/enable-langs (&rest langs)
  "Enable LANGS."
  (mapcar (lambda (lang)
            (setq halidom-proglang-enabled-list
                  (cons lang halidom-proglang-enabled-list)))
          langs))

(when *is-mac*
  (halidom/enable-langs 'asm 'c-c++ 'common-lisp 'clojure 'groovy
                        'java 'javascript 'markdown 'scala 'python
                        'ruby 'web))

;; Begin conf Group
;; Nginx

;; YAML Mode

(use-package yaml-mode
    :mode (("\\.yml\\'" . yaml-mode)
           (".clang-tidy\\'" . yaml-mode)))

;; End conf Group

;; Begin ASM
(when (halidom/proglang-enabled-p 'asm)
  ;; ASM Hooks
  (defun halidom/asm-setup ()
    (setq tab-stopp-list (number-sequence 2 60 2)))
  
  (defvar asm-colon-has-space nil)
  
  (defun halidom/asm-colon-check-space ()
    (setq asm-colon-has-space nil)
    (when (and (not (null (char-after)))
               (member (string (char-after)) '(" " "\t")))
      (setq asm-colon-has-space t)))
  
  (defun halidom/asm-colon-delete-space ()
    (unless asm-colon-has-space
      (call-interactively 'delete-horizontal-space)))
  
  (advice-add 'asm-colon :before 'halidom/asm-colon-check-space)
  (advice-add 'asm-colon :after  'halidom/asm-colon-delete-space)
  ;; ASM Mode
  (use-package asm-mode
    :mode (("\\.64sa\\'" . asm-mode)
           ("\\.64da\\'" . asm-mode)
           ("\\.32sa\\'" . asm-mode)
           ("\\.32da\\'" . asm-mode))
    :config
    (progn
      (define-key asm-mode-map (kbd "C-j") 'newline)))
  ;; NASM Mode
  (use-package nasm-mode
    :init
    (add-hook 'nasm-mode-hook #'halidom/asm-setup)
    :mode
    (("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode))
    :bind ((:map nasm-mode-map
                 ("C-j" . newline)
                 (":" . asm-colon))))
  
  ;; ASM x86 Lookup
  (use-package x86-lookup
    :init
    (progn
      (when (straight-check-package "pdf-tools")
        (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)))
    :config
    (setq x86-lookup-pdf
          "~/Dropbox/Documents/Books/ASM/x86-manual/325462-sdm-vol-1-2abcd-3abcd.pdf"))
  ) ;; End ASM

;; Begin C/C++
(when (halidom/proglang-enabled-p 'c-c++)
  ;; Cmake
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode)
           ("\\.cmake\\'" . cmake-mode))
    :init
    (with-eval-after-load 'projectile
      (add-to-list 'projectile-project-root-files-top-down-recurring
                   "compile_commands.json"))
  
    (defun cmake-build-compilation-database ()
      (interactive)
      (let* ((project-directory (cond (projectile-project-root
                                       projectile-project-root)
                                      ((eq 'major-mode 'dired-mode)
                                       dired-directory)
                                      (t default-directory)))
             (build-directory (f-join project-directory "build"))
             (compiledb (f-join build-directory "compile_commands.json")))
        (when (f-exists? (f-join project-directory "CMakeLists.txt"))
          (if-not (f-exists? build-directory)
              (mkdir build-directory))
          (with-temp-buffer
            (shell-command "make clean")
            (cd build-directory)
            (cmake-command-run "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-directory))
          (if (f-exists? compiledb)
              (make-symbolic-link (f-slash project-directory) build-directory t)))))
  
    (defun cmake-mode-dash-docsets ()
      (setq-local dash-plugin-keywords '("cmake")))
  
    :hook (cmake-mode . cmake-mode-dash-docsets))
  
  ;; Make Comint
  (defun make-command (command)
    (interactive "P")
    (compilation-start
     (read-shell-command "Make command: " "make " command)))
  
  ;; Cquery
  
  (use-package cquery
      :commands lsp-cquery-enable
      :if (executable-find "cquery")
      :custom
      (cquery-executable (executable-find "cquery"))
      (cquery-extra-init-params '(:index (:comments 2)
                                  :cacheFormat "msgpack"
                                  :completion (:detailedLabel t)))
      :init
      (defun cquery/enable ()
        "Enable cquery in the workspace."
        (condition-case nil
            (lsp-cquery-enable)
          (user-error "%s" "cquery didn't work")))
  
      :hook
      (c-mode-common . cquery/enable)
      ((c-mode c++-mode) . halidom/flycheck-enable-hook)
  
      :config
  
      ;; Alternatively, use lsp-ui-peek interface
      ;; (setq cquery-sem-highlight-method 'font-lock)
      ;; alternatively
      (setq cquery-sem-highlight-method 'overlay)
  
      ;; For rainbow semantic highlighting
      (with-eval-after-load 'lsp-ui-mode
        (lsp-ui-peek-find-custom 'base "$cquery/base")
        (lsp-ui-peek-find-custom 'callers "$cquery/callers")
        (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
  
      (cquery-use-default-rainbow-sem-highlight)
  
      (with-eval-after-load 'projectile
          (add-to-list 'projectile-project-root-files-top-down-recurring
                       ".cquery")))
  
  ;; Clangd
  (use-package lsp-clangd
      :straight (lsp-clangd
                 :type git
                 :repo "emacs-lsp/lsp-clangd"
                 :host github)
      :commands (lsp-clangd-c-enable
                 lsp-clangd-c++-enable
                 lsp-clangd-objc-enable)
      :custom
      (lsp-clangd-executable  (executable-find "clangd"))
  
      :if  (executable-find "clangd")
      :hook
      ((c-mode . lsp-clangd-c-enable)
       (c++-mode . lsp-clangd-c++-enable)
       (objc-mode . lsp-clangd-objc-enable)))
  
  ;; Clang Tidy
  (use-package flycheck-clang-tidy
      :after flycheck
      :if (executable-find "clang-tidy")
      :init
      (defun clang-tidy/enable ()
        (when (and (-non-nil projectile-project-root)
                   (string= projectile-project-type "cmake"))
          (when (fboundp 'flycheck-mode)
            (flycheck-clang-tidy-setup))))
      (add-hook 'c-mode-common-hook 'clang-tidy/enable))
  
  ;; Rtags
  ;; rtags
  
  (use-package rtags
      :if (executable-find "rdm")
      :hook
      ((c-mode . rtags-start-process-unless-running)
       (c++-mode . rtags-start-process-unless-running)
       (objc-mode . rtags-start-process-unless-running)))
  
  
  ;; Ivy rtags
  (use-package ivy-rtags
      :requires (rtags))
  ;; llvm
  (use-package llvm-mode
      :straight nil
      :load-path "~/.emacs.d/opt/llvm-mode"
      :init
      (require 'llvm-mode)
      (require 'tablegen-mode))
  
)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-top-down-recurring
                 "compile_commands.json"))

  (defun cmake-build-compilation-database ()
    (interactive)
    (let* ((project-directory (cond (projectile-project-root
                                     projectile-project-root)
                                    ((eq 'major-mode 'dired-mode)
                                     dired-directory)
                                    (t default-directory)))
           (build-directory (f-join project-directory "build"))
           (compiledb (f-join build-directory "compile_commands.json")))
      (when (f-exists? (f-join project-directory "CMakeLists.txt"))
        (if-not (f-exists? build-directory)
            (mkdir build-directory))
        (with-temp-buffer
          (shell-command "make clean")
          (cd build-directory)
          (cmake-command-run "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-directory))
        (if (f-exists? compiledb)
            (make-symbolic-link (f-slash project-directory) build-directory t)))))

  (defun cmake-mode-dash-docsets ()
    (setq-local dash-plugin-keywords '("cmake")))

  :hook (cmake-mode . cmake-mode-dash-docsets))

;; Begin CLisp
(when (halidom/proglang-enabled-p 'common-lisp)
  ;; CL Hooks
  (defun common-lisp-style ()
    "Styleguide for common lisp."
    (if (fboundp 'paredit-mode)
        (paredit-mode +1))
    (if (fboundp 'highlight-symbol-mode)
        (highlight-symbol-mode +1))
    (if *is-mac*
        (setq-local dash-plugin-keywords '("lisp"))))
  
  (add-hook 'lisp-mode-hook #'common-lisp-style)
  ;; Slime
  (use-package slime
    :commands slime
    :defines (slime-complete-symbol*-fancy
              slime-completion-at-point-functions)
    :init
    (progn
      (setq slime-contribs
              '(slime-asdf
        			  slime-fancy
  			        slime-indentation
        			  slime-sbcl-exts
        			  slime-scratch)
  	          inferior-lisp-program "sbcl"
        	    ;; enable fuzzy matching in code buffer and SLIME REPL
        	    slime-complete-symbol*-fancy t
        	    slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
  
      (defun slime-disable-smartparens ()
        (smartparens-strict-mode -1)
        (turn-off-smartparens-mode))
  
      (add-hook 'slime-repl-mode-hook #'slime-disable-smartparens)))
) ;; End CLisp

(defun common-lisp-style ()
  "Styleguide for common lisp."
  (if (fboundp 'paredit-mode)
      (paredit-mode +1))
  (if (fboundp 'highlight-symbol-mode)
      (highlight-symbol-mode +1))
  (if *is-mac*
      (setq-local dash-plugin-keywords '("lisp"))))

(add-hook 'lisp-mode-hook #'common-lisp-style)

;; Begin Clojure
(when (halidom/proglang-enabled-p 'clojure)
  ;; Clojure Mode
  (use-package clojure-mode
    :custom (clojure-indent-style :always-indent)
  
    :mode (("\\.edn$"   . clojure-mode)
           ("\\.cljs$"  . clojurescript-mode)
           ("\\.cljx$"  . clojurex-mode)
           ("\\.cljsc$" . clojurec-mode))
  
    :hook ((clojure-mode . enable-paredit-mode)
           (clojure-mode . show-paren-mode))
  
    :config
  
    (defun halidom/clj-dash-docsets ()
      "Keywords for Clojure Docsets via Dash.app."
      (setq-local dash-plugin-keywords '("clojure")))
  
    (defun halidom/clj-style-compojure ()
      "Indendation for macros defined in compojure routing framework.
  
       See https://github.com/weavejester/compojure/wiki/Emacs-indentation."
  
      (define-clojure-indent
          (defroutes 'defun)
          (GET 2)
          (POST 2)
          (PUT 2)
          (DELETE 2)
          (HEAD 2)
          (ANY 2)
          (OPTIONS 2)
          (PATCH 2)
          (rfn 2)
          (let-routes 1)
          (context 2)))
  
  
    (defun halidom/clj-style-om-next ()
      "Indendation for om-next macros."
        (put-clojure-indent 'defui '(1 nil nil (1)))
        (put-clojure-indent 'dom/div 1))
  
    (defun halidom/clj-style-guide ()
      "Styleguide for clojure."
      (halidom/clj-style-compojure)
      (halidom/clj-style-om-next))
  
    (add-hook 'clojure-mode-hook #'halidom/clj-dash-docsets)
    (add-hook 'clojure-mode-hook #'halidom/clj-style-guide))
  ;; Clojure Mode Extra Font Locking
  (use-package clojure-mode-extra-font-locking
    :requires clojure-mode
    :config
    (defun halidom/clj-extra-font-locking ()
      (require 'clojure-mode-extra-font-locking))
  
    (add-hook 'clojure-mode-hook #'halidom/clj-extra-font-locking))
  ;; IDE Widget
  (defcustom halidom-clojure-ide 'cider
    "Select the interactive development environment to use
     in clojure mode. Note that Emacs must be restarted when
     the value of this variable is modified for the change
     to take effect."
    :type '(symbol)
    :options '(cider inf-clojure))
  ;; CIDER
  (use-package cider
    :if (eq halidom-clojure-ide 'cider)
    :init
    (with-eval-after-load 'clojure-mode
      (add-hook 'clojure-mode-hook 'cider-mode))
    :custom
    (cider-repl-history-file "~/.emacs.d/cider-history")
    (cider-repl-use-clojure-font-lock t)
    (cider-repl-result-prefix ";; => ")
    (cider-repl-wrap-history t)
    (cider-repl-history-size 3000)
    (cider-show-error-buffer nil)
    (nrepl-hide-special-buffers t)
    :hook (((cider-mode cider-repl-mode) . eldoc-mode)
           ((cider-mode cider-repl-mode) . company-mode)
           ((cider-mode cider-repl-mode) .
            cider-company-enable-fuzzy-completion)
           (cider-repl-mode . subword-mode)))
  ;; Inf Clojure
  (use-package inf-clojure
    :if (eq halidom-clojure-ide 'inf-clojure)
    :init
    (with-eval-after-load 'clojure-mode
      (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))
    :config
    (defun figwheel-repl ()
      (interactive)
      (inf-clojure "lein figwheel")))
  ;; Clj Refactor
  (use-package clj-refactor
    :init
    (defun halidom/clj-refactor-enable ()
      "Enable clj-refactor in clojure-mode."
      (clj-refactor-mode 1)
      ;; For adding reuire/use/import statements
      (yas-minor-mode 1)
      ;; Unbinds `cider-macroexpand-1'
      (cljr-add-keybindings-with-prefix "C-c C-m"))
  
    :hook ((clojure-mode) . halidom/clj-refactor-enable))
  
  ;; Leiningen
  ;; Elein
  (use-package elein
    :if (executable-find "lein")
    :straight t)
  ;; Cljsbuild
  (use-package cljsbuild-mode
    :if (executable-find "lein")
    :hook ((clojure-mode clojurescript-mode) . cljsbuild-mode))
) ;; End Clojure

;; Begin emacs-lisp
(defun emacs-lisp-style ()
  (paredit-mode 1)
  (highlight-symbol-mode 1)
  (setq lisp-indent-function 'common-lisp-indent-function))

(defun halidom/elisp-dash-docsets ()
  (setq-local dash-plugin-keywords '("elisp")))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-style)
(add-hook 'emacs-lisp-mode-hook #'halidom/elisp-dash-docsets)

;; Cask
;; Cask Mode
(use-package cask-mode)
;; Flycheck cask
(use-package flycheck-cask
  :init
  (defun flycheck-cask/enable ()
    (when (eq projectile-project-type 'emacs-cask)
      (flycheck-cask-setup)))
  (defun flycheck-cask/disable-dir-locals ()
      (when (string= ".dir-locals.el" (buffer-file-name))
        (set (make-variable-buffer-local 'flycheck-mode) nil)))

  :hook
  (flycheck-mode . flycheck-cask/enable)
  (flycheck-mode . flycheck-cask/disable-dir-locals))

;; Cask Package Toolset
(use-package cask-package-toolset)
;; End emacs-lisp

(use-package groovy-mode
  :mode  "\\.gradle\\'"
  :config
  (defun groovy-style ()
    (setq groovy-indent-offset 2
          tab-width 4
          indent-tabs-mode nil
          c-indent-comments-syntactically-p t))
  (add-hook 'groovy-mode-hook #'groovy-style))

;; Begin Java
(when (halidom/proglang-enabled-p 'java)
  ;; Java Backend
  (defcustom halidom-java-backend 'meghanada
    "Select a backend to use when opening a *.java file."
    :type '(symbol)
    :options '(meghanada ensime eclim lsp))
  ;; Java Hooks
  (defun java-mode-style ()
    (c-set-offset 'arglist-close '0)
    (setq indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2))
  
  (defun java-mode-dash-docsets-hook ()
    (setq-local dash-plugin-keywords '("java" "gradle" "groovy")))
  
  ;; Enable Hooks
  (add-hook 'java-mode-hook 'java-mode-style)
  
  (if *is-mac*
    (add-hook 'java-mode-hook 'java-mode-dash-docsets-hook))
  ;; Eclim
  (use-package eclim
    :if (eq halidom-java-backend 'eclim)
  
    :custom
    (eclimd-autostart-with-default-workspace t)
    (eclim-autostart nil)
    (eclim-wait-for-process t))
  
  ;; Meghanada
  (use-package meghanada
    :if (eq halidom-java-backend 'meghanada)
  
    :commands meghanada-mode
  
    :bind
    ((:map meghanada-mode-map
  	       ("C-S-t" . meghanada-switch-testcase)
  	       ("M-RET" . meghanada-local-variable)
  	       ("M-r"   . meghanada-reference)
  	       ("M-t"   . meghanada-typeinfo)
  	       ("C-z"   . hydra-meghanada/body)))
  
    :hook
    ((java-mode . meghanada-mode)
     (java-mode . flycheck-mode))
  
    :custom
    (meghanada-server-remote-debug t)
    (meghanada-javac-xlint "-Xlint:all,-processing")
  
    :config
    (defhydra hydra-meghanada (:hint nil :exit t)
      "
         ^Edit^                          ^Tast or Task^
         ^^^^^^---------------------------------------------
         _f_: meghanada-compile-file     _m_: meghanada-restart
         _c_: meghanada-compile-project  _t_: meghanada-run-task
         _o_: meghanada-optimize-import  _j_: meghanada-run-junit-test-case
         _s_: meghanada-switch-test-case _J_: meghanada-run-junit-class
         _v_: meghanada-local-variable   _R_: meghanada-run-junit-recent
         _i_: meghanada-import-all       _r_: meghanada-reference
         _q_: exit                       _T_: meghanada-typeinfo
         "
        ("f" meghanada-compile-file)
        ("m" meghanada-restart)
  
        ("c"  meghanada-compile-project)
        ("o"  meghanada-optimize-import)
        ("s"  meghanada-switch-test-case)
        ("v"  meghanada-local-variable)
        ("i"  meghanada-import-all)
  
  
        ("t"  meghanada-run-task)
        ("T"  meghanada-typeinfo)
        ("j"  meghanada-run-junit-test-case)
        ("J"  meghanada-run-junit-class)
        ("R"  meghanada-run-junit-recent)
        ("r"  meghanada-reference)
  
        ("q" exit)
        ("z" nil "leave")))
  
  
  ;; Autodisass Java Bytecode
  (use-package autodisass-java-bytecode)
  ;; Gradle Mode
  (use-package gradle-mode
    :if (executable-find "gradle")
    :hook
    ((java-mode . gradle-mode)))
) ;;End Java

;; Begin JavaScript
(when (halidom/proglang-enabled-p 'javascript)
  ;; Set up backend
  (defcustom halidom-js-backend 'lsp
    "The backend to use with JavaScript and friends.
  Supported symbols are `tern' and `lsp'. If enabled,
  the `halidom-js-lsp-clients' variable can be used to
  customize the enabled clients."
    :type '(choice
            (const :tag "Use tern." tern)
            (const :tag "Use lsp." lsp)
            (const :tag "I don't like nice things." nil)))
  
  (defcustom halidom-js-lsp-clients '(javascript-typescript)
    "List of defined stdio LSP clients to enable for use in JavaScript.
  Supported values are `javascript-typescript', `javascript-flow',
  and `typescript'.
  By default, only `javascript-typescript' is enabled. This is because
  it's the only one of the three I've gotten to work so far."
    :type '(list symbol))
  
  (defcustom halidom-js-lsp-modes '(js typescript rjsx)
    "List of JavaScript major modes which should support `lsp-mode'."
    :type '(list symbol))
  
  
  ;; Tern
  ;; Tern Backend
  (use-package tern)
  
  (use-package company-tern
      :requires (tern company))
  
  (defun js-setup-tern ()
    "Setup the JS backend for Tern."
    (let ((completion-backends '(company-tern)))
      (if-not (executable-find "tern")
          (error "%s" "Executable `tern' could not be found in PATH.")
        (require 'tern)
        (require 'company-tern)
        (set (make-local-variable 'company-backends) completion-backends)
        (message "%s" "Using Tern backend for JavaScript."))))
  ;; LSP
  
  ;; LSP Backend
  
  ;; LSP JS Typescript
  (use-package lsp-javascript-typescript
      :if (executable-find "javascript-typescript-langserver")
      :commands lsp-javascript-typescript-enable)
  
  ;; LSP JS Flow
  (use-package lsp-javascript-flow
      :if (executable-find "flow-language-server")
      :commands lsp-javascript-flow-enable)
  
  ;; LSP Typescript
  (use-package lsp-typescript
      :straight (lsp-typescript
                 :type git
                 :files ("lsp-typescript.el")
                 :host github
                 :repo "emacs-lsp/lsp-javascript")
      :if (executable-find "typescript-language-server")
      :commands lsp-typescript-enable)
  
  (defun js-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))
  
  (defun js-setup-lsp ()
    "Set up the JS backend for LSP.
  
  More specifically, enable lsp clients listed in `halidom-js-lsp-clients'
  for major modes listed in `halidom-js-lsp-modes'."
  
    (cl-flet ((lsp-client-function (client)
                (intern (concat "lsp-" (symbol-name client) "-enable")))
              (lsp-client-req-pkg (client)
                (require `,(intern
                            (concat "lsp-" (symbol-name client))))))
  
      ;;  Fix issue with `javascript-typescript-langserver' completion.
      (when (member 'javascript-typescript halidom-js-lsp-clients)
        (make-local-variable 'company-transformers)
        (push 'js-company-transformer company-transformers))
  
      (cl-loop
         for lsp-client in halidom-js-lsp-clients
         for enable-func = (lsp-client-function lsp-client)
         do
           (lsp-client-req-pkg lsp-client)
           (funcall enable-func))))
  ;; Setup backend
  
  (defun js-setup-backend-function ()
    "Return the setup function corresponding to the symbol
    assigned to `halidom-js-backend.' "
    (pcase halidom-js-backend
      (`tern #'js-setup-tern)
      (`lsp  #'js-setup-lsp)))
  
  (defun js-setup-backend ()
    "Set up the JavaScript Backend."
    (let ((backend-function (js-setup-backend-function)))
      (funcall backend-function)))
  
  
  ;; js2 mode
  
  (use-package js2-mode
    :custom
    (js-indent-level 2)
    :mode (("\\.js\\'" . js2-mode)
           ("\\.mjs\\'" . js2-mode))
    :interpreter "node"
    :init
  
    :hook ((js-mode . js-setup-backend)
           (js-mode . halidom/flycheck-enable-hook)))
  ;; JSON Mode
  (use-package json-mode
    :mode "\\.json\\'"
    :init
    (defun json-mode-style ()
      "Styleguide for JSON Mode."
      (set (make-local-variable 'js-indent-level) 2))
  
      (defun json-mode-faces ()
      (let ((foreground (face-foreground 'font-lock-variable-name-face)))
        (face-remap-add-relative 'font-lock-keyword-face
                                 `(:slant normal :foreground ,foreground))))
    (defun json-inhibit-message ()
        (set (make-local-variable 'inhibit-message) t))
  
    :hook
    (json-mode . json-mode-style)
    (json-mode . json-mode-faces)
    (json-mode . json-inhibit-message))
  
  ;; RJSX Mode
  (use-package rjsx-mode
    :mode "\\.jsx\\'"
    :hook ((rjsx-mode-hook . js-setup-backend)
           (rjsx-mode-hook . halidom/flycheck-enable-hook)))
  ;; Tide Mode
  (use-package typescript-mode
      :custom
      (typescript-indent-level 2)
      :hook
      (typescript-mode . subword-mode)
      (typescript-mode . js-setup-backend))
  
  
  (use-package tide
    :after (:all typescript-mode flycheck company)
    :demand t
    :init
    ;; (require 'typescript-mode)
    (defun setup-tide-mode ()
      "Enable Tide Mode for Typescript."
      (tide-setup)
      (when (fboundp 'flycheck-mode)
        (flycheck-add-mode 'typescript-tslint 'typescript-mode)
        (flycheck-mode +1)
        (setq flycheck-check-syntax-automatically '(save mode-enabled)))
      (eldoc-mode +1))
  
      :hook
      (before-save . tide-format-before-save)
      (typescript-mode . setup-tide-mode)
      (typescript-mode . tide-hl-identifier-mode))
  ;; Coffeescript
  
  ;; Node.js
  ;; Node Modules
  (use-package add-node-modules-path
    :if (executable-find "node")
    :init
    (progn
      (add-hook 'js-mode-hook #'add-node-modules-path)))
  ;; Npm
  (use-package npm-mode
      :if (executable-find "npm")
      :init
      (el-patch-feature npm-mode)
      (el-patch-defun npm-mode--exec-process (cmd)
        "Execute a process running CMD."
        (message (concat "Running " cmd))
        (compile cmd (el-patch-add t)))
  
      (defun npm-mode-npm-install-global (dep)
        (interactive "sEnter package name: ")
        (npm-mode--exec-process (format "npm i -g %s" dep)))
  
      :config
      (npm-global-mode)
  
      :hook
      (js-mode . npm-mode))
  ;; Yarn
  (use-package yarn-mode
      :if (executable-find "yarn"))
  ;; Node Version Manager (nvm)
  (use-package nvm
    :if (executable-find "nvm"))
  
  ;; Indium
  (use-package indium
      :bind (:map indium-interaction-mode-map
                  ("C-M-b" . indium-eval-buffer))
      :init
      (el-patch-feature indium-nodejs)
      (with-eval-after-load 'indium-nodejs
        (el-patch-defun indium-nodejs--process-filter (process output)
          "Filter function for PROCESS.
  Append OUTPUT to the PROCESS buffer, and parse it to detect the
  socket URL to connect to."
          ;; Append output to the process buffer
          (with-current-buffer (process-buffer process)
            (goto-char (point-max))
            (insert output)
            (goto-char (point-min))
            (el-patch-add (ansi-color-apply-on-region (point-min) (point-max))))
  
          (when (string-match-p "Debugger listening on" output)
            (ignore-errors
              (indium-nodejs--connect-to-process process output)))))
  
  
      (cl-defun chrome-debugger-launch (&optional (port "3000") (host "localhost") (type "http") )
        "Launch a chromium debugger process on HOST using PORT and protocol TYPE.
  
      Note this will kill any running instances of Chromium."
          (interactive (list
                        (read-string "Port: " "3000")
                        (read-string "Host: " "localhost")
                        (read-string "Type: " "http")))
          (if-not (featurep 'secrets)
              (require 'secrets))
  
          (if (boundp 'google-api-key)
              (setenv "GOOGLE_API_KEY" google-api-key))
          (if (boundp 'google-api-key)
              (setenv "GOOGLE_DEFAULT_CLIENT_ID" google-default-client-id))
          (if (boundp 'google-default-client-secret)
              (setenv "GOOGLE_DEFAULT_CLIENT_SECRET" google-default-client-secret))
  
          (let ((process-names (--> (process-list)
                                    (mapcar (lambda (p) (process-name p)) it)))
                (process (get-process "chromium"))
                (ip (concat type "://" host ":" port))
                (chromium-program browse-url-chromium-program))
            (if (shell-command-to-list "pgrep \"Chromium\"")
                (shell-command "killall Chromium"))
            (if (process-live-p process)
                (kill-process process))
            (start-process "chromium" (get-buffer-create "chromium")
                           chromium-program "--remote-debugging-port=9222" ip)))
    :hook
    (js-mode . indium-interaction-mode))
  
  
) ;; End JavaScript

(defcustom halidom-js-backend 'lsp
  "The backend to use with JavaScript and friends.
Supported symbols are `tern' and `lsp'. If enabled,
the `halidom-js-lsp-clients' variable can be used to
customize the enabled clients."
  :type '(choice
          (const :tag "Use tern." tern)
          (const :tag "Use lsp." lsp)
          (const :tag "I don't like nice things." nil)))

(defcustom halidom-js-lsp-clients '(javascript-typescript)
  "List of defined stdio LSP clients to enable for use in JavaScript.
Supported values are `javascript-typescript', `javascript-flow',
and `typescript'.
By default, only `javascript-typescript' is enabled. This is because
it's the only one of the three I've gotten to work so far."
  :type '(list symbol))

(defcustom halidom-js-lsp-modes '(js typescript rjsx)
  "List of JavaScript major modes which should support `lsp-mode'."
  :type '(list symbol))


;; Tern
;; Tern Backend
(use-package tern)

(use-package company-tern
    :requires (tern company))

(defun js-setup-tern ()
  "Setup the JS backend for Tern."
  (let ((completion-backends '(company-tern)))
    (if-not (executable-find "tern")
        (error "%s" "Executable `tern' could not be found in PATH.")
      (require 'tern)
      (require 'company-tern)
      (set (make-local-variable 'company-backends) completion-backends)
      (message "%s" "Using Tern backend for JavaScript."))))
;; LSP

;; LSP Backend

;; LSP JS Typescript
(use-package lsp-javascript-typescript
    :if (executable-find "javascript-typescript-langserver")
    :commands lsp-javascript-typescript-enable)

;; LSP JS Flow
(use-package lsp-javascript-flow
    :if (executable-find "flow-language-server")
    :commands lsp-javascript-flow-enable)

;; LSP Typescript
(use-package lsp-typescript
    :straight (lsp-typescript
               :type git
               :files ("lsp-typescript.el")
               :host github
               :repo "emacs-lsp/lsp-javascript")
    :if (executable-find "typescript-language-server")
    :commands lsp-typescript-enable)

(defun js-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun js-setup-lsp ()
  "Set up the JS backend for LSP.

More specifically, enable lsp clients listed in `halidom-js-lsp-clients'
for major modes listed in `halidom-js-lsp-modes'."

  (cl-flet ((lsp-client-function (client)
              (intern (concat "lsp-" (symbol-name client) "-enable")))
            (lsp-client-req-pkg (client)
              (require `,(intern
                          (concat "lsp-" (symbol-name client))))))

    ;;  Fix issue with `javascript-typescript-langserver' completion.
    (when (member 'javascript-typescript halidom-js-lsp-clients)
      (make-local-variable 'company-transformers)
      (push 'js-company-transformer company-transformers))

    (cl-loop
       for lsp-client in halidom-js-lsp-clients
       for enable-func = (lsp-client-function lsp-client)
       do
         (lsp-client-req-pkg lsp-client)
         (funcall enable-func))))
;; Setup backend

(defun js-setup-backend-function ()
  "Return the setup function corresponding to the symbol
  assigned to `halidom-js-backend.' "
  (pcase halidom-js-backend
    (`tern #'js-setup-tern)
    (`lsp  #'js-setup-lsp)))

(defun js-setup-backend ()
  "Set up the JavaScript Backend."
  (let ((backend-function (js-setup-backend-function)))
    (funcall backend-function)))

(defcustom halidom-js-backend 'lsp
  "The backend to use with JavaScript and friends.
Supported symbols are `tern' and `lsp'. If enabled,
the `halidom-js-lsp-clients' variable can be used to
customize the enabled clients."
  :type '(choice
          (const :tag "Use tern." tern)
          (const :tag "Use lsp." lsp)
          (const :tag "I don't like nice things." nil)))

(defcustom halidom-js-lsp-clients '(javascript-typescript)
  "List of defined stdio LSP clients to enable for use in JavaScript.
Supported values are `javascript-typescript', `javascript-flow',
and `typescript'.
By default, only `javascript-typescript' is enabled. This is because
it's the only one of the three I've gotten to work so far."
  :type '(list symbol))

(defcustom halidom-js-lsp-modes '(js typescript rjsx)
  "List of JavaScript major modes which should support `lsp-mode'."
  :type '(list symbol))

;; Begin Python
(when (halidom/proglang-enabled-p 'python)
  ;; Python Mode
  (use-package python-mode
    :init
    (when (executable-find "ipython3")
      (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "-i"))
    :config
    (defun python-dash-docsets ()
      (setq-local dash-plugin-keywords
                  '("python" "django" "twisted" "sphinx"
                    "flask" "tornado" "sqlalchemy" "numpy"
                    "scipy" "salt" "pandas" "matplotlib"
                    "cvp")))
    (when *is-mac*
      (add-hook #'python-dash-docsets)))
  
  ;; Pyenv Mode
  (use-package pyenv-mode
      :if (executable-find "pyenv")
      :init
      (add-to-list 'exec-path (user-home ".pyenv" "shims"))
      (setenv "WORKON_HOME" (file-name-as-directory (user-home ".pyenv" "versions")))
      (pyenv-mode)
      :bind
      ("C-x p e" . pyenv-activate-current-project)
      ("C-x p s" . pyenv-mode-set)
      :commands
  
      (pyenv-mode-set pyenv-mode-unset pyenv-mode-versions)
  
      :config
  
      (defvar pyenv-current-version nil nil)
  
      (defun pyenv-init()
        "Initialize pyenv's current version to the global one."
        (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
          (message (concat "Setting pyenv version to " global-pyenv))
          (pyenv-mode-set global-pyenv)
          (setq pyenv-current-version global-pyenv)))
  
     (add-hook 'after-init-hook 'pyenv-init)
  
      (progn
        (with-eval-after-load 'projectile
          (defun projectile-pyenv-mode-set ()
            "Set pyenv version matching project name."
            (let ((project (projectile-project-name)))
              (if (member project (pyenv-mode-versions))
                  (pyenv-mode-set project)
                (pyenv-mode-unset))))
  
          (add-hook 'projectile-after-switch-project-hook
                    'projectile-pyenv-mode-set))
  
        ;; http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
       (defun pyenv-activate-current-project ()
         "Automatically activates pyenv version if .python-version file exists."
         (interactive)
         (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
           (if python-version-directory
               (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
                      (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
                 (pyenv-mode-set pyenv-current-version)
                 (message (concat "Setting virtualenv to " pyenv-current-version))))))))
  ;; Pyenv Mode Auto
  (use-package pyenv-mode-auto)
  ;; Virtualenvwrapper
  (use-package virtualenvwrapper
      :init
      (setq venv-dirlookup-names '(".pyenv" ".venv"))
      (setq venv-location (getenv "WORKON_HOME"))
      :config
      (when (fboundp 'projectile-mode)
        (setq projectile-switch-project-action
              '(lambda ()
                (venv-projectile-auto-workon)
                (projectile-find-file)))))
  
  ;; Pyvenv
  (use-package pyvenv
      :requires virtualenvwrapper
      :init
    (pyvenv-mode))
  ;; LSP Python
  (use-package lsp-python
      :commands lsp-python-enable
      ;; Requires python-language-server
      :if (executable-find "pyls")
      :init
      (add-hook 'python-mode-hook #'lsp-python-enable)
  
      (defun python//enable-flycheck ()
        (when (fboundp 'flycheck-mode)
          (flycheck-mode +1)))
  
      (add-hook 'python-mode-hook 'python//enable-flycheck))
  ;; Pip Requirements
  (use-package pip-requirements
      :straight t)
  ;; Pydoc
  (use-package pydoc
      :straight t)
  ;; EIN
  (use-package ein
      :config
      (setq ein:use-smartrep t))) ;; End Python

;; Begin Ruby
(when (halidom/proglang-enabled-p 'ruby)
  ;; Ruby Mode
  (use-package ruby-mode
    :mode "\\.rb\\'"
    :interpreter "ruby"
    :config
  
    (defun halidom/ruby-dash-docsets ()
      (setq-local dash-plugin-keywords '("ruby" "rails")))
  
  
    (add-hook 'ruby-mode-hook #'halidom/ruby-dash-docsets))
  
  ;; Enhanced Ruby Mode
  (use-package enh-ruby-mode
      :after ruby-mode
      :demand t
      :mode "\\.rb\\'"
      :config
      (add-hook 'enh-ruby-mode-hook #'halidom/ruby-dash-docsets))
  ;; RVM
  (use-package rvm
    :init
    (rvm-use-default))
  ;; Robe Mode
  (use-package robe
      :after (:all ruby-mode rvm enh-ruby-mode)
      :demand t
      :init
    ;; https://github.com/dgutov/robe#integration-with-rvmel
      (defadvice inf-ruby-console-auto (before
                                        activate-rvm-for-robe
                                        activate)
        (rvm-activate-corresponding-ruby))
    (defun halidom/robe-mode-enable ()
      (robe-mode)
      (with-eval-after-load 'company
        (push 'company-robe company-backends)))
    (add-to-hooks #'halidom/robe-mode-enable
                  '(ruby-mode-hook enh-ruby-mode-hook)))
  
  ;; Yard Mode
  (use-package yard-mode
      :after (:all ruby-mode enh-ruby-mode)
      :demand t
      :init
      (add-to-hooks #'yard-mode '(ruby-mode-hook enh-ruby-mode-hook))
      (add-to-hooks #'eldoc-mode '(ruby-mode-hook enh-ruby-mode-hook)))
   ) ;; End Ruby

;; Begin Scala
(when (halidom/proglang-enabled-p 'scala)
  ;; Scala Mode
  (use-package scala-mode
    :interpreter
    ("scala" . scala-mode))
  ;; Scala SBT Mode
  (use-package sbt-mode
    :commands (sbt-start sbt-command)
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)) 
) ;: End Scala

;; Begin Web Configuration
(when (halidom/proglang-enabled-p 'web)
   ;; Web Mode
   (use-package web-mode
     :bind (:map web-mode-map
                 ;; I should rebind this because it conficts
                 ;; with `company-select-next-or-abort'
                 ("M-n" . web-mode-tag-match))
     :mode
     (("\\.phtml\\'"      . web-mode)
      ("\\.tpl\\.php\\'"  . web-mode)
      ("\\.twig\\'"       . web-mode)
      ("\\.html\\'"       . web-mode)
      ("\\.htm\\'"        . web-mode)
      ("\\.[gj]sp\\'"     . web-mode)
      ("\\.as[cp]x?\\'"   . web-mode)
      ("\\.eex\\'"        . web-mode)
      ("\\.erb\\'"        . web-mode)
      ("\\.mustache\\'"   . web-mode)
      ("\\.handlebars\\'" . web-mode)
      ("\\.hbs\\'"        . web-mode)
      ("\\.eco\\'"        . web-mode)
      ("\\.ejs\\'"        . web-mode)
      ("\\.djhtml\\'"     . web-mode)
      ("\\.tsx\\'"        . web-mode))
   
     :config
     (setq web-mode-engines-alist
           '(("php" . "\\.phtml\\'")
             ("blade" . "\\.blade\\'")))
   
     (defun halidom/web-mode-enable ()
       "Enabled web-mode options."
       (setq web-mode-enable-auto-pairing t
             web-mode-enable-css-colorization t
             web-mode-enable-block-face t
             web-mode-enable-part-face t
             web-mode-enable-comment-keywords t
             web-mode-enable-heredoc-fontification t
             web-mode-enable-current-element-highlight t
             web-mode-enable-current-column-highlight t)
       )
   
   
     (defun halidom/web-mode-tide ()
       "Enable tide-mode in Typescript tsx buffers."
       (when (string-equal "tsx" (file-name-extension buffer-file-name))
         (setup-tide-mode)
         (flycheck-add-mode 'typescript-tslint 'web-mode)))
   
     (add-hook 'web-mode-hook #'halidom/web-mode-tide)
   
     (defun halidom/web-mode-indent ()
       "Indentation settings for web-mode."
       (setq web-mode-markup-indent-offset 2
             web-mode-code-indent-offset 2
             web-mode-style-padding 1
             web-mode-script-padding 1
             web-mode-block-padding 0
             web-mode-comment-style 2))
   
     (defun halidom/web-mode-style ()
       "Styleguide for web-mode."
       (halidom/web-mode-enable)
       (halidom/web-mode-indent))
   
     (defun halidom/web-mode-flycheck ()
       (when (fboundp 'flycheck-mode)
         (flycheck-mode +1)))
   
     (add-hook 'web-mode-hook #'halidom/web-mode-flycheck)
   
     (add-hook 'web-mode-hook #'halidom/web-mode-style)
   
     (defun halidom/web-dash-docsets ()
       (setq-local dash-plugin-keywords
                   '("css" "html" "javascript" "react")))
     (when *is-mac*
       (add-hook 'web-mode-hook #'halidom/web-dash-docsets)))
   ;; Tagedit Mode
   (use-package tagedit
     :init
     (defun halidom/tagedit-enable ()
       (tagedit-add-experimental-features)
       (tagedit-mode +1))
   
     (add-hook 'html-mode-hook #'halidom/tagedit-enable)
   
     :diminish tagedit-mode)
   ;; Htmlize
   (use-package htmlize
     :straight t)
   ;; CSS
   ;; Begin CSS
   
   (use-package css-mode
     :commands (css-expand-statment css-contrac-statement)
     :custom (css-indent-offset )
     :init
     (defun css-expand-statment ()
       (interactive)
       (save-excursion
         (end-of-line)
         (search-backward "{")
         (forward-char 1)
         (while (or (eobp) (not (looking-at "}")))
           (let ((beg (point)))
             (newline)
             (search-forward ";")
             (indent-region beg (point))))
         (newline)))
   
     (defun css-contrac-statement ()
       "Contract CSS Block"
       (interactive)
       (end-of-line)
       (search-backward "{")
       (while (not (looking-at "}"))
         (join-line -1)))
   
     :config
     (progn
       (add-hook 'css-mode-hook (lambda ()
                                  (setq css-indent-offset 2)))))
   
   ;; Less Mode
   (use-package less-mode
       :mode "\\.less\\'")
   ;; Sass Mode
   (use-package sass-mode
     :mode "\\.sass\\'")
   ;; SCSS Mode
   (use-package scss-mode
       :mode "\\.scss\\'")
   ;; CSS LSP
   (use-package lsp-css
       :if (executable-find "css-language-server")
       :commands (lsp-css-enable
                  lsp-less-enable
                  lsp-scss-enable
                  lsp-scss-enable))
   ;; CSS Backends
   (defun css-mode-setup ()
     (when (eq major-mode 'css-mode)
       ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
       ;; fires for scss-mode because scss-mode is derived from css-mode)
       (require 'lsp-css)
       (lsp-css-enable)))
   
   (defun less-mode-setup ()
     (require 'lsp-css)
     (lsp-less-enable))
   
   (defun scss-mode-setup ()
     (require 'lsp-css)
     (lsp-scss-enable))
   
   (add-hook 'css-mode-hook #'css-mode-setup)
   (add-hook 'less-mode-hook #'less-mode-setup)
   (add-hook 'sass-mode-hook #'scss-mode-setup)
   (add-hook 'scss-mode-hook #'scss-mode-setup)
   ;; End CSS
   ;; Emmet Mode
   (use-package emmet-mode
     :init
     (add-to-hooks 'emmet-mode
                   '(css-mode-hook html-mode-hook web-mode-hook)))
   ;; Company Tern
   (use-package tern
     :if (executable-find "tern"))
   
   (use-package company-tern
       :after (:all company tern)
       :demand t)
   ;; Company Web
   (use-package company-web
     :after (:all company web-mode)
     :demand t
     :init
     (defun halidom/company-web-mode-enable ()
       "Enable company web-mode backends."
       (let ((backends '(company-web-html company-yasnippet company-files)))
         (when (featurep 'company-tern)
           (advice-add 'company-tern :before
                       #'(lambda (&rest _)
                           (if (equal major-mode 'web-mode)
                               (let ((web-mode-cur-language
                                      (web-mode-language-at-pos)))
                                 (if (or (string= web-mode-cur-language "javascript")
                                         (string= web-mode-cur-language "jsx"))
                                     (unless tern-mode (tern-mode))
                                   (if tern-mode (tern-mode -1)))))))
           (setq backends (push 'company-tern backends)))
         (set (make-local-variable 'company-backends) backends)))
   
   
     (add-hook 'web-mode-hook #'halidom/company-web-mode-enable))
   
   ;; Web LSP
   (use-package lsp-html
       :if (executable-find "html-languageserver")
       :commands (lsp-html-enable)
       :init
       (require 'lsp-html)
       (defun html-lsp-setup ()
         (when (string-equal "html" (file-name-extension buffer-file-name))
           (lsp-html-enable)
           (flycheck-add-mode 'html-tidy 'web-mode)))
       :hook (web-mode . html-lsp-setup))
   
   

) ;; End Web Configuration

;; Begin Markdown Configuration
(when (halidom/proglang-enabled-p 'markdown)
  ;; Markdown Mode
  
  ;; Markdown-Mode+
  (use-package markdown-mode+
    :straight t)
  ;; Markdown Toc
  (use-package markdown-toc
    :straight t
    :defer t)
  ;; Markdownfmt
  (use-package markdownfmt
    :straight t
    :defer t
    :commands (markdown-format-buffer markdownfmt-enable-on-save)
    :bind (:map markdown-mode-map
  	      ("C-c C-f" . markdown-format-buffer))
    :config
    (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save))
  ;; Livedown
  (use-package livedown
      :straight (livedown
                 :type git
                 :host github
                 :repo "shime/emacs-livedown")
      :custom (livedown-open nil)
      :init
      (when (featurep 'xwidget)
        (require 'xwidget)
        (defadvice livedown-preview (after livedown-preview-after activate)
           (xwidget-webkit-browse-url "http://localhost:1337"))))
) ;; End Markdown Configuration

(use-package scimax
  :straight nil
  :if (file-directory-p (emacs-etc-dir "local" "scimax"))
  :custom
  (scimax-dir (emacs-etc-dir "local" "scimax"))
  :load-path "etc/local/scimax"
  :init
  (require 'scimax-ivy)
  (require 'scimax-latex)
  (require 'scimax-utils))

;;; halidom.el
