(setq org-agenda-files '("~/Dropbox/org/todos/TODOs.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(safe-local-variable-values
   (quote
    ((eval bibtex-set-dialect
	   (quote bibtex))
     (org-image-actual-width)
     (eval bibtex-set-dialect
	   (quote biblatex))
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (org-ref-default-bibliography . ftv108-term-paper\.bib)
     (org-ref-pdf-directory concat
			    (file-name-directory
			     (buffer-file-name))
			    "research")
     (org-latex-hyperref-template)
     (eval sh-set-shell "zsh")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(spaceline-highlight-face ((t (:background "#5F7F5F" :foreground "#1F1E20" :inherit (quote mode-line))))))
