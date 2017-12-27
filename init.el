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

;; Use Package Configuration
(straight-use-package 'use-package)

(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

(straight-use-package 'diminish)
(straight-use-package 'bind-key)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

;; Default packages
(straight-use-package 'cl)
(straight-use-package 'dash)

(eval-when-compile
  (require 'cl)
  (require 'dash)
  (require 'subr-x))

(straight-use-package
  `(org
    :host github
    :repo "emacsmirror/org"
    :local-repo-name org
    :files ("lisp/*.el" "contrib/lisp/*.el")))

(use-package org
  :bind
  (("C-c a" . org-agenda))
  :init
  (progn
    (defun chaffin--org-git-version ()
      (let ((default-directory
        (concat
          (or user-emacs-directory
             (expand-file-name "~/.emacs.d/"))
          "straight/repos/org")))
        (if (executable-find "git")
          (with-temp-buffer
            (call-process "git" nil '(t nil) nil
            "rev-parse" "--short" "HEAD")
          (if (> (buffer-size) 0)
            (string-trim (buffer-string))
          "revision unknown"))
        "Git not installed!")))
      (defalias #'org-git-version #'chaffin--org-git-version)
     (defun org-release () "9.1.3")

     (provide 'org-version))


  :config
  (setq org-insert-heading-respect-content t
	org-startup-indented t))


(defun chaffin--load-config (&optional org-config-file)
  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t)
  (org-babel-load-file
    (concat
      (or user-emacs-directory
          (expand-file-name "~/.emacs.d/"))
      (or org-config-file
          "chaffin.org"))))

(chaffin--load-config)





