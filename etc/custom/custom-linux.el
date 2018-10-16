(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1b2b34" "#ec5f67" "#99c794" "#fac863" "#6699cc" "#c594c5" "#6699cc" "#c0c5ce"])
 '(bmkp-last-as-first-bookmark-file "/home/jacobchaffin/.emacs.d/var/bmkp/current-bookmark.el")
 '(browse-url-chromium-program "chromium")
 '(cfw:display-calendar-holidays nil)
 '(cfw:fchar-horizontal-line 9552)
 '(cfw:fchar-junction 9580)
 '(cfw:fchar-left-junction 9568)
 '(cfw:fchar-right-junction 9571)
 '(cfw:fchar-top-junction 9574)
 '(cfw:fchar-top-left-corner 9556)
 '(cfw:fchar-top-right-corner 9559)
 '(cfw:fchar-vertical-line 9553)
 '(cfw:org-capture-template
   '("c" "calfw2org" entry
     (file "agenda/schedule.org")
     "*  %?
 %(cfw:org-capture-day)"))
 '(cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap t)
 '(company-box-enable-icon nil)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates nil)
 '(company-transformers nil)
 '(counsel-gtags-auto-update t t)
 '(counsel-gtags-ignore-case t t)
 '(diary-file "/home/jacobchaffin/org/diary.org")
 '(ediff-diff-options "-w")
 '(enable-recursive-minibuffers t)
 '(eshell-modify-global-environment t)
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes nil)
 '(ivy-initial-inputs-alist nil t)
 '(ivy-re-builders-alist '((t . ivy-prescient-re-builder)) t)
 '(ivy-sort-max-size 50000)
 '(ivy-todo-file "/home/jacobchaffin/Dropbox/org/agenda/ivy-todo.org" t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(lsp-inhibit-message t)
 '(lsp-message-project-root-warning t t)
 '(lsp-project-blacklist '("^/ssh:" "node_modules"))
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-width 100)
 '(lsp-ui-sideline-show-symbol nil)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(no-littering-etc-directory "/home/jacobchaffin/.emacs.d/etc" t)
 '(org-bullets-bullet-list '("› "))
 '(org-confirm-babel-evaluate nil)
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-emphasis-markers t)
 '(org-insert-heading-respect-content t)
 '(org-pretty-entities t)
 '(org-projectile-capture-template "** TODO %?
    :PROPERTIES:
    :CREATED: %U
    :END:" t)
 '(org-projectile-per-project-filepath "TODOs.org" t)
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(org-tags-column 0)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-indexing-method 'alien)
 '(projectile-switch-project-action 'projectile-dired)
 '(safe-local-variable-values
   '((org-noter-default-notes-file-names "ling103.org")
     (port . "22")
     (remote-path "~/courses/cs131")
     (user . "classbin")
     (host . "lnxsrv07seas.ucla.edu")))
 '(skeletor-completing-read-function 'ivy-completing-read)
 '(skeletor-project-directory "/home/jacobchaffin/Developer/Projects")
 '(skeletor-python-bin-search-path '("/usr/local/bin" "/usr/bin"))
 '(skeletor-user-directory "/home/jacobchaffin/.emacs.d/etc/skeletons/")
 '(which-key-enable-extended-define-key t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t (:foreground "Gray10" :weight bold :inherit (cfw:face-toolbar)))))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold :inherit (cfw:face-toolbar)))))
