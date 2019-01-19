;;; -*- lexical-binding: t ; -*-
;;; early-init.el -- Emacs early initialization file
;;
;; Copyright (c) 2018 Jacob Chaffin
;; Author Jacob Chaffin
;;
;; Homepage: https://github.com/.emacs.d.git
;;
;; Commentary:
;;
;; From the Emacs 27.1 NEWS:
;;
;;
;;    Emacs can now be configured using an early init file.
;;    The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;;    loaded very early in the startup process: before graphical elements
;;    such as the tool bar are initialized, and before the package manager
;;    is initialized.  The primary purpose is to allow customizing how the
;;    package system is initialized given that initialization now happens
;;    before loading the regular init file (see below).
;;    PKG_COMMENTARY
;;
;;
;;; Code:

(setq package-enable-at-startup nil)


(defun feature-disabled-cli (arg args)
  (cond ((= (length args) 0) nil)
	      ((equal (car args) arg) t)
	      (t (feature-disabled-cli arg (cdr args)))))

(defun feature-disabled-env (var)
  (let ((env (getenv var)))
    (and env (= (string-to-number env) 1))))

(defun feature-check (env-func cli-func)
  (let ((env-disabled (funcall env-func))
	      (cli-disabled (funcall cli-func command-line-args)))
    (if (or env-disabled cli-disabled) nil t)))

(add-to-list 'command-switch-alist
	     '("--no-literate" .
               (lambda (_) (pop command-line-args-left))))

(defun literate-disabled-env ()
  (feature-disabled-env "HALIDOM_NO_LITERATE"))

(defalias #'literate-disabled-cli
  (apply-partially #'feature-disabled-cli "--no-literate"))

(defun check-literate ()
  "Check whether literate mode is enabled."
  (feature-check #'literate-disabled-env #'literate-disabled-cli))

(defvar use-literate-p (check-literate)
  "If non-nil, disable tangling of `halidom-literate-config-file.'")


;;
;; Chemacs - Emacs Profile Switcher v0.1
;;
;; INSTALLATION
;;
;; Install this file as ~/.emacs . Next time you start Emacs it will create a
;; ~/.emacs-profiles.el , with a single "default" profile
;;
;;     (("default" . ((user-emacs-directory . "~/.emacs.d"))))
;;
;; Now you can start Emacs with `--with-profile' to pick a specific profile. A
;; more elaborate example:
;;
;;     (("default"                      . ((user-emacs-directory . "~/emacs-profiles/plexus")))
;;      ("spacemacs"                    . ((user-emacs-directory . "~/github/spacemacs")
;;                                         (server-name . "spacemacs")
;;                                         (custom-file . "~/.spacemacs.d/custom.el")
;;                                         (env . (("SPACEMACSDIR" . "~/.spacemacs.d"))))))
;;
;; If you want to change the default profile used (so that, for example, a
;; GUI version of Emacs uses the profile you want), you can also put the name
;; of that profile in a ~/.emacs-profile file

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; this must be here to keep the package system happy, normally you do
;; `package-initialize' for real in your own init.el
;; (package-initialize)

;;; Code:
(defvar chemacs-profiles-path "~/.emacs-profiles.el")
(defvar chemacs-default-profile-path "~/.emacs-profile")

(when (not (file-exists-p chemacs-profiles-path))
  (with-temp-file chemacs-profiles-path
    (insert "((\"default\" . ((user-emacs-directory . \"~/.emacs.d\"))))")))

(defvar chemacs-emacs-profiles
  (with-temp-buffer
    (insert-file-contents chemacs-profiles-path)
    (goto-char (point-min))
    (read (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chemacs-detect-default-profile ()
  (if (file-exists-p chemacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents chemacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer)) ))
    "default"))

(defun chemacs-get-emacs-profile (profile)
  (cdr (assoc profile chemacs-emacs-profiles)))

(defun chemacs-emacs-profile-key (key &optional default)
  (alist-get key (chemacs-get-emacs-profile chemacs-current-emacs-profile)
             default))

(defun chemacs-load-profile (profile)
  (when (not (chemacs-get-emacs-profile profile))
    (error "No profile `%s' in %s" profile chemacs-profiles-path))
  (setq chemacs-current-emacs-profile profile)
  (let* ((emacs-directory (file-name-as-directory
                           (chemacs-emacs-profile-key 'user-emacs-directory)))
         (init-file       (expand-file-name "init.el" emacs-directory))
         (custom-file-    (chemacs-emacs-profile-key 'custom-file init-file))
         (server-name-    (chemacs-emacs-profile-key 'server-name)))
    (setq user-emacs-directory emacs-directory)

    ;; Allow multiple profiles to each run their server
    ;; use `emacsclient -s profile_name' to connect
    (when server-name-
      (setq server-name server-name-))

    ;; Set environment variables, these are visible to init-file with getenv
    (mapcar (lambda (env)
              (setenv (car env) (cdr env)))
            (chemacs-emacs-profile-key 'env))

    ;; Start the actual initialization
    (load init-file)

    ;; Prevent customize from changing ~/.emacs (this file), but if init.el has
    ;; set a value for custom-file then don't touch it.
    (when (not custom-file)
      (setq custom-file custom-file-)
      (load custom-file))))

(defun chemacs-check-command-line-args (args)
  (if args
      ;; Handle either `--with-profile profilename' or
      ;; `--with-profile=profilename'
      (let ((s (split-string (car args) "=")))
        (cond ((equal (car args) "--with-profile")
               ;; This is just a no-op so Emacs knows --with-profile
               ;; is a valid option. If we wait for
               ;; command-switch-alist to be processed then
               ;; after-init-hook has already run.
               (add-to-list 'command-switch-alist
                            '("--with-profile" .
                              (lambda (_) (pop command-line-args-left))))
               ;; Load the profile
               (chemacs-load-profile (cadr args)))

              ;; Similar handling for `--with-profile=profilename'
              ((equal (car s) "--with-profile")
               (add-to-list 'command-switch-alist `(,(car args) . (lambda (_))))
               (chemacs-load-profile (mapconcat 'identity (cdr s) "=")))

              (t (chemacs-check-command-line-args (cdr args)))))

    ;; If no profile given, load the "default" profile
    (chemacs-load-profile (chemacs-detect-default-profile))))

;; Check for a --with-profile flag and honor it; otherwise load the
;; default profile.
(chemacs-check-command-line-args command-line-args)

(provide 'early-init)

;;; early-init.el ends here
