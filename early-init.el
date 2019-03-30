;;; early-init.el --- Emacs early initialization file  -*- lexical-binding: t ; -*-
;;
;; Copyright (c) 2018 Jacob Chaffin
;; Author Jacob Chaffin
;;
;; Homepage: https://github.com/.emacs.d.git
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

(defun literate-disabled-env ()
  (feature-disabled-env "DISABLE_LITERATE"))

(defalias #'literate-disabled-cli
  (apply-partially #'feature-disabled-cli "--no-literate"))

(defun check-literate ()
  "Check whether literate mode is enabled."
  (feature-check #'literate-disabled-env #'literate-disabled-cli))

(add-to-list 'command-switch-alist
	     '("--no-literate" .
               (lambda (_) (pop command-line-args-left))))

(defvar use-literate-p (check-literate)
"If non-nil, disable tangling of `dotemacs-literate-config-file'")
;;; early-init.el ends here
