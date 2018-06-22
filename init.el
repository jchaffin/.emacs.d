;;; init.el -- Emacs Initialization File
;;
;; Copyright (c) 2018 Jacob Chaffin
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Keywords: emacs, .emacs.d, elisp, straight-el
;; Homepage: https://github.com/jchaffin/.emacs.d
;; Package-Requires: ((emacs "26"))
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
;; [3] https://github.com/jchaffin/.emacs.d/blob/master/chaffin.org#literate-programming
;; [4] http://orgmode.org/worg/org-contrib/babel/

;;; Code:


(setq halidom-init-file load-file-name)

;; Straight Initialization
(setq halidom--straight-defaults '(:branch "develop"
                                  :bootstrap-version 2
                                  :profiles ((mac . "mac-versions.el")
      					     (linux . "linux-versions.el")
  					     (windows . "windows-versions.el")
					     (nil . "default.el"))))

(setq halidom-package-dir
      (file-name-as-directory
       (expand-file-name "straight" user-emacs-directory)))

(defun halidom//straight-bootstrap ()
  (let ((bootstrap-file (expand-file-name "bootstrap.el" halidom-package-dir)))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defvar halidom-bootstrap-hook nil)

(defun halidom//straight-initialize (&optional branch profiles current)
  "Bootstrap the straight package manager. Optionally, clone repository BRANCH and 
configure an alist of PROFILES with CURRENT as the initial profile in use."
  (let ((certfile "/usr/local/etc/libressl/cert.pem")
        (repository-branch (or branch (plist-get halidom--straight-defaults :branch)))
        (profiles (or profiles (plist-get halidom--straight-defaults :profiles)))
        (current-profile (or current (plist-get halidom--straight-defaults :current-profile))))
    (when (file-exists-p certfile)
      (require 'gnutls)
      (add-to-list 'gnutls-trustfiles certfile))
    (setq straight-repository-branch repository-branch)
    (setq straight-profiles profiles)
    (setq straight-current-profile current-profile))
  (funcall #'halidom//straight-bootstrap)
  (run-hooks 'halidom-bootstrap-hook))


(defvar halidom-use-package-init-hook nil)

(defun halidom//straight-use-package ()
  ;; Clone use-package dependencies
  (straight-use-package 'diminish)
  (straight-use-package 'bind-key)
  ;; Now clone the `use-package' library
  (straight-use-package 'use-package)
  ;; Straight integration of `use-package'.
  ;; Allow built-in packages to be configured by `use-package'.
  (setq straight-use-package-version 'straight)
  ;; And enable by default.
  (setq straight-use-package-by-default t)
  ;; TODO: Figure out what this is doing
  (setq straight-built-in-pseudo-packages
	'(emacs browse-url artist-mode winner-mode xwidget))
  ;; Defer by default
  (setq use-package-always-defer t)
  ;; Advice system and package lazy-loading
  ;; https://github.com/raxod502/el-patch#lazy-loading-packages
  (straight-use-package 'el-patch)
  (run-hooks 'halidom-use-package-init-hook))


(defun halidom//org ()
  (add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
  (require 'init-org)) 

(defun halidom/straight ()
  (add-hook 'halidom-bootstrap-hook #'halidom//straight-use-package)
  (add-hook 'halidom-use-package-init-hook #'halidom//org)
  (halidom//straight-initialize))

;;; Literate config

(defvar halidom--literate-config-file "chaffin.org"
  "The *.org file containing the source code responsible for
  declaration and configuration of third-party packages, as well as
  any settings and customizations defined in this GNU Emacs
  distribution.")

(defvar literate-init-file
  (expand-file-name halidom--literate-config-file user-emacs-directory)
  "The absolute path of `literate-config-file.'")


(defun halidom/literate (&optional config-file init-server)
  "If USER-CONFIG-FILE is passed as an argument, then tangle.
Else use the value of `halidom--literate-config-file'."
  (let* ((file (or config-file halidom--literate-config-file))
         (dir (or user-emacs-directory default-directory))
	 (path (expand-file-name file dir)))
    (when init-server
      (require 'server)
      (start-server))
    
    (if (file-exists-p path)
        (org-babel-load-file path)
      (error "%s" "No configuration file."))))


;; Initialize
(when use-straight-p (halidom/straight))
(when use-literate-p (halidom/literate))
