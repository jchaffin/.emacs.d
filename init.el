(setq package-enable-at-startup nil)

;; Boostrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(declare-function straight-use-package "straight.el")


(straight-use-package 'el-patch)
(straight-use-package 'dash)

(when (eq system-type 'darwin)
  (straight-use-package '(org
			:host github
			:repo "jchaffin/org-mode"
			:local-repo-name org
			:files ("lisp/*.el" "contrib/lisp/*.el"))))

(straight-use-package '(org-beautify-theme
			:host github
			:repo "jchaffin/org-beautify-theme"
			:uptream (:host github
				  :repo "jonnay/org-beautify-theme")))

(straight-use-package '(use-package
			 :host github
			 :repo "raxod502/use-package"
			 :upstream (:host github
				    :repo "jwiegley/use-package")))
(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'subr-x)

(use-package org
  :commands (org-version)
  :bind (("C-c a" . org-agenda))
  :functions jchaffin--org-git-version
  :init
  (progn
   (defun jchaffin--org-git-version ()
     (let ((default-directory (concat user-emacs-directory
				      "straight/repos/org/")))
       (if (executable-find "git")
	   (with-temp-buffer
	     (call-process "git" nil '(t nil) nil
			   "rev-parse" "--short" "HEAD")
	     (if (> (buffer-size) 0)
		 (string-trim (buffer-string))
	       "revision unknown"))
	 "git not available")))
   (defalias #'org-git-version #'jchaffin--org-git-version)
   (defun org-release () "N/A")
   (provide 'org-version)
   (with-eval-after-load 'org
     (defalias #'org-git-version #'jchaffin--org-git-version)))

  :config
  (setq org-insert-heading-respect-content t
	org-startup-indented t))


(defun chaffin--org-init ()
  (setq org-src-fontify-natively t
	org-confirm-babel-evaluate nil
	org-src-preserve-indentation t)
  (org-babel-load-file (concat user-emacs-directory "chaffin.org")))

(add-hook 'after-init-hook #'chaffin--org-init)
