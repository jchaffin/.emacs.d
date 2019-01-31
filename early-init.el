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

;;; early-init.el ends here
