;;; -*- lexical-binding: t ; -*-
;;; early-init.el -- Emacs early initialization file
;;
;; Copyright (c) 2017 Jacob Chaffin
;; Author Jacob Chaffin
;; Keywords:  PKG_KEYWORDS
;; Homepage: https://github.com/.emacs.d
;; Package Requires:  PKG_DEPENDENCIES
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;; ;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
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

(add-to-list 'command-switch-alist '("--no-straight" .
                                     (lambda (_) (pop command-line-args-left))))

(add-to-list 'command-switch-alist '("--no-literate" .
                                     (lambda (_) (pop command-line-args-left))))


(defun feature-disabled--cli (flag args)
  (cond ((= (length args) 0) nil)
	((equal (car args) flag) t)
	(t (feature-disabled--cli flag (cdr args)))))

(defalias #'straight-disabled-cli (apply-partially #'feature-disabled--cli "--no-straight"))
(defalias #'literate-disabled-cli (apply-partially #'feature-disabled--cli "--no-literate"))

(defun feature-disabled--env (var)
  (let ((env (getenv var)))
    (and env (= (string-to-number env) 1))))

(defun straight-disabled-env () (feature-disabled--env "HALIDOM_NO_STRAIGHT"))
(defun literate-disabled-env () (feature-disabled--env "HALIDOM_NO_LITERATE"))

(defun feature-check (env-func cli-func)
  (let ((env-disabled (funcall env-func))
	(cli-disabled (funcall cli-func command-line-args)))
    (if (or env-disabled cli-disabled) nil t)))

(defun check-straight () (feature-check #'straight-disabled-env #'straight-disabled-cli))
(defun check-literate () (feature-check #'literate-disabled-env #'literate-disabled-cli))


(defvar use-straight-p (check-straight)
  "If non-nil, inhibit package.el and use straight.el as the default package manager.
This variable is non-nil by default. To set to nil, either pass `--no-straight' as a
command line argument at startup, or set the environment variable `HALIDOM_NO_STRAIGHT' to
a positive numerical value.")

(defvar use-literate-p (check-literate)
  "If non-nil, inhibit package.el and use straight.el as the default package manager.
This variable is non-nil by default. To set to nil, either pass `--no-literate' as a
command line argument at startup, or set the environment variable `HALIDOM_NO_LITERATE' to
a positive numerical value.")

;;; early-init.el ends here
