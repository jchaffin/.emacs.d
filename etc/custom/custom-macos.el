(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fill-break-at-separators nil)
 '(LaTeX-item-indent nil)
 '(TeX-auto-save t)
 '(TeX-error-overview-open-after-TeX-run t)
 '(TeX-interactive-mode nil t)
 '(TeX-parse-self t)
 '(TeX-syntactic-comment t t)
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
 '(auto-save-no-message t)
 '(aw-background nil)
 '(aw-keys '(97 115 100 102 103 104 106 107 108))
 '(bibtex-BibTeX-entry-alist
   '(("Article" "Article in Journal"
      (("author")
       ("title" "Title of the article (BibTeX converts it to lowercase)"))
      (("journal")
       ("year"))
      (("volume" "Volume of the journal")
       ("number" "Number of the journal (only allowed if entry contains volume)")
       ("pages" "Pages in the journal")
       ("month")
       ("note")))
     ("InProceedings" "Article in Conference Proceedings"
      (("author")
       ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)"))
      (("booktitle" "Name of the conference proceedings")
       ("year"))
      (("editor")
       ("volume" "Volume of the conference proceedings in the series")
       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
       ("series" "Series in which the conference proceedings appeared")
       ("pages" "Pages in the conference proceedings")
       ("month")
       ("address")
       ("organization" "Sponsoring organization of the conference")
       ("publisher" "Publishing company, its location")
       ("note")))
     ("Conference" "Article in Conference Proceedings"
      (("author")
       ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)"))
      (("booktitle" "Name of the conference proceedings")
       ("year"))
      (("editor")
       ("volume" "Volume of the conference proceedings in the series")
       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
       ("series" "Series in which the conference proceedings appeared")
       ("pages" "Pages in the conference proceedings")
       ("month")
       ("address")
       ("organization" "Sponsoring organization of the conference")
       ("publisher" "Publishing company, its location")
       ("note")))
     ("InCollection" "Article in a Collection"
      (("author")
       ("title" "Title of the article in book (BibTeX converts it to lowercase)")
       ("booktitle" "Name of the book"))
      (("publisher")
       ("year"))
      (("editor")
       ("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("type" "Word to use instead of \"chapter\"")
       ("chapter" "Chapter in the book")
       ("pages" "Pages in the book")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("address")
       ("note")))
     ("InBook" "Chapter or Pages in a Book"
      (("author" nil nil 0)
       ("editor" nil nil 0)
       ("title" "Title of the book")
       ("chapter" "Chapter in the book"))
      (("publisher")
       ("year"))
      (("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("type" "Word to use instead of \"chapter\"")
       ("address")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("pages" "Pages in the book")
       ("note")))
     ("Proceedings" "Conference Proceedings"
      (("title" "Title of the conference proceedings")
       ("year"))
      nil
      (("booktitle" "Title of the proceedings for cross references")
       ("editor")
       ("volume" "Volume of the conference proceedings in the series")
       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
       ("series" "Series in which the conference proceedings appeared")
       ("address")
       ("month")
       ("organization" "Sponsoring organization of the conference")
       ("publisher" "Publishing company, its location")
       ("note")))
     ("Book" "Book"
      (("author" nil nil 0)
       ("editor" nil nil 0)
       ("title" "Title of the book"))
      (("publisher")
       ("year"))
      (("volume" "Volume of the book in the series")
       ("number" "Number of the book in a small series (overwritten by volume)")
       ("series" "Series in which the book appeared")
       ("address")
       ("edition" "Edition of the book as a capitalized English word")
       ("month")
       ("note")))
     ("Booklet" "Booklet (Bound, but no Publisher)"
      (("title" "Title of the booklet (BibTeX converts it to lowercase)"))
      nil
      (("author")
       ("howpublished" "The way in which the booklet was published")
       ("address")
       ("month")
       ("year")
       ("note")))
     ("PhdThesis" "PhD. Thesis"
      (("author")
       ("title" "Title of the PhD. thesis")
       ("school" "School where the PhD. thesis was written")
       ("year"))
      nil
      (("type" "Type of the PhD. thesis")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("MastersThesis" "Master's Thesis"
      (("author")
       ("title" "Title of the master's thesis (BibTeX converts it to lowercase)")
       ("school" "School where the master's thesis was written")
       ("year"))
      nil
      (("type" "Type of the master's thesis (if other than \"Master's thesis\")")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("TechReport" "Technical Report"
      (("author")
       ("title" "Title of the technical report (BibTeX converts it to lowercase)")
       ("institution" "Sponsoring institution of the report")
       ("year"))
      nil
      (("type" "Type of the report (if other than \"technical report\")")
       ("number" "Number of the technical report")
       ("address")
       ("month")
       ("note")))
     ("Manual" "Technical Manual"
      (("title" "Title of the manual"))
      nil
      (("author")
       ("organization" "Publishing organization of the manual")
       ("address")
       ("edition" "Edition of the manual as a capitalized English word")
       ("month")
       ("year")
       ("note")))
     ("Unpublished" "Unpublished"
      (("author")
       ("title" "Title of the unpublished work (BibTeX converts it to lowercase)")
       ("note"))
      nil
      (("month")
       ("year")))
     ("Misc" "Miscellaneous" nil nil
      (("author")
       ("title" "Title of the work (BibTeX converts it to lowercase)")
       ("howpublished" "The way in which the work was published")
       ("month")
       ("year")
       ("note")))
     ("Online" "Online Resource"
      (("author" nil nil 0)
       ("editor" nil nil 0)
       ("title" nil nil nil)
       ("year" nil nil 1)
       ("date" nil nil 1)
       ("url" nil nil nil))
      nil
      (("subtitle" nil nil)
       ("titleaddon" nil nil)
       ("language" nil nil)
       ("howpublished" nil nil)
       ("type" nil nil)
       ("version" nil nil)
       ("note" nil nil)
       ("organization" nil nil)
       ("location" nil nil)
       ("date" nil nil)
       ("month" nil nil)
       ("year" nil nil)
       ("addendum" nil nil)
       ("pubstate" nil nil)
       ("doi" nil nil)
       ("eprint" nil nil)
       ("eprintclass" nil nil)
       ("eprinttype" nil nil)
       ("url" nil nil)
       ("urldate" nil nil)))))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/var/bmkp/current-bookmark.el")
 '(browse-url-chromium-program "/Applications/Chromium.app/Contents/MacOS/Chromium")
 '(cdlatex-insert-auto-labels-in-env-templates t)
 '(cfw:display-calendar-holidays nil nil nil "Customized with use-package calfw")
 '(cfw:fchar-horizontal-line 9473 nil nil "Customized with use-package calfw")
 '(cfw:fchar-junction 9547 nil nil "Customized with use-package calfw")
 '(cfw:fchar-left-junction 9507 nil nil "Customized with use-package calfw")
 '(cfw:fchar-right-junction 9515 nil nil "Customized with use-package calfw")
 '(cfw:fchar-top-junction 9519 nil nil "Customized with use-package calfw")
 '(cfw:fchar-top-left-corner 9487 nil nil "Customized with use-package calfw")
 '(cfw:fchar-top-right-corner 9491 nil nil "Customized with use-package calfw")
 '(cfw:fchar-vertical-line 9475 nil nil "Customized with use-package calfw")
 '(cfw:org-capture-template
   '("c" "calfw2org" entry
     (file "agenda/schedule.org")
     "*  %?
 %(cfw:org-capture-day)"))
 '(cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap t nil "Customized with use-package calfw")
 '(cider-repl-history-file "~/.emacs.d/cider-history" t)
 '(cider-repl-history-size 3000 t)
 '(cider-repl-result-prefix ";; => " t)
 '(cider-repl-use-clojure-font-lock t t)
 '(cider-repl-wrap-history t t)
 '(cider-show-error-buffer nil t)
 '(clojure-indent-style :always-indent t)
 '(company-box-enable-icon nil)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates nil)
 '(company-transformers nil)
 '(counsel-gtags-auto-update t)
 '(counsel-gtags-ignore-case t)
 '(cquery-executable "/usr/local/bin/cquery" t)
 '(cquery-extra-init-params
   '(:index
     (:comments 2)
     :cacheFormat "msgpack" :completion
     (:detailedLabel t)) t)
 '(css-indent-offset nil t)
 '(describe-char-unidata-list
   '(name old-name general-category decomposition numeric-value iso-10646-comment))
 '(diary-file "/Users/jacobchaffin/Dropbox/org/diary.org")
 '(dired-sidebar-should-follow-file nil t)
 '(dired-sidebar-theme 'none t)
 '(ediff-diff-options "-w" t)
 '(enable-recursive-minibuffers t)
 '(eshell-modify-global-environment t)
 '(eshell-prompt-regexp "λ ")
 '(fci-rule-color "#202020")
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes nil)
 '(ggtags-highlight-tag nil t)
 '(global-auto-revert-mode nil)
 '(global-display-line-numbers-mode nil)
 '(ivy-initial-inputs-alist nil t)
 '(ivy-re-builders-alist '((t . ivy-prescient-re-builder)) t)
 '(ivy-sort-max-size 50000)
 '(ivy-todo-file "/Users/jacobchaffin/Dropbox/org/agenda/ivy-todo.org" t)
 '(ivy-use-selectable-prompt nil)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2 t)
 '(latex/no-fill-environments
   '("equation" "equation*" "align" "align*" "forest" "forest*" "tabular" "tikzpicture") t)
 '(livedown-open nil t)
 '(lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd" t)
 '(lsp-inhibit-message t)
 '(lsp-message-project-root-warning t t)
 '(lsp-project-blacklist '("^/ssh:" "node_modules"))
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-width 100)
 '(lsp-ui-sideline-show-symbol nil)
 '(magic-latex-enable-block-align nil t)
 '(magic-latex-enable-block-highlight t t)
 '(magic-latex-enable-inline-image nil t)
 '(magic-latex-enable-pretty-symbols t t)
 '(magic-latex-enable-suscript nil t)
 '(meghanada-javac-xlint "-Xlint:all,-processing")
 '(meghanada-server-remote-debug t)
 '(merlin-command 'opam t)
 '(merlin-error-after-save nil t)
 '(monky-process-type 'cmdserver t)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(no-littering-etc-directory "/Users/jacobchaffin/.emacs.d/etc" t)
 '(nrepl-hide-special-buffers t t)
 '(org-agenda-files
   '("~/Dropbox/org/todos/TODOs.org" "/Users/jacobchaffin/Dropbox/courses/cs131/TODOs.org" "/Users/jacobchaffin/Dropbox/courses/ling103/TODOs.org" "/Users/jacobchaffin/Dropbox/courses/ling165b/TODOs.org"))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-time-leading-zero t)
 '(org-attach-auto-tag "attach" t)
 '(org-babel-uppercase-example-markers t)
 '(org-bullets-bullet-list '("‣" "•") nil nil "Customized with use-package org-bullets")
 '(org-catch-invisible-edits nil)
 '(org-confirm-babel-evaluate nil)
 '(org-edit-src-persistent-message nil)
 '(org-ellipsis "   ")
 '(org-gcal-file-alist
   '(("jchaffin@g.ucla.edu" . "~/Dropbox/org/agenda/schedule.org")))
 '(org-hide-emphasis-markers t)
 '(org-highlight-latex-and-related '(latex))
 '(org-journal-enable-agenda-integration t)
 '(org-onenote-token-file
   "/Users/jacobchaffin/.emacs.d/etc/org-onenote-oauth2.plstore")
 '(org-pandoc-options '((standalone . t)))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-projectile-capture-template "** TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:" t)
 '(org-projectile-per-project-filepath "TODOs.org" t)
 '(org-ref-bibliography-notes "/Users/jacobchaffin/Dropbox/org/ref/refnotes.org")
 '(org-ref-bibtex-hydra-key-binding "j")
 '(org-ref-completion-library 'org-ref-ivy-cite)
 '(org-ref-default-bibliography '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(org-ref-pdf-directory "/Users/jacobchaffin/Dropbox/org/papers/pdfs/")
 '(org-ref-show-broken-links t)
 '(org-ref-show-citation-on-enter t)
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'other-window)
 '(org-startup-indented t)
 '(org-tags-column 0)
 '(pdf-annot-activate-created-annotations t t)
 '(pdf-view-display-size 'fit-page)
 '(pdf-view-resize-factor 1.1)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-indexing-method 'turbo-alien)
 '(projectile-switch-project-action 'projectile-dired)
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "gradle run")
     (projectile-project-name . "jmmplus")
     (projectile-project-name . "cs131-grammars")
     (projectile-project-type . "gradle")
     (checkdoc-package-keywords-flag)
     (projectile-project-name . "jmm")
     (org-latex-hyperref-template)
     (org-ref-pdf-directory . "/Users/jacobchaffin/Dropbox/courses/cs131/pdfs/")
     (org-ref-pdf-directory "/Users/jacobchaffin/Dropbox/courses/cs131/pdfs/")
     (projectile-project-name . "cs131-gramars")
     (projectile-project-type quote dune)
     (projectile-project-name . "hello-world-dune")
     (org-highlight-latex-and-related quote
                                      (latex))
     (org-ref-pdf-directory . "/Users/jacobchaffin/Dropbox/courses/ling103/pdfs")
     (org-ref-pdf-directory . "/Users/jacobchaffin/Dropbox/courses/ling165b/materials/readings")
     (projectile-project-name . "Phonetics")
     (projectile-project-name . "Programming Languages")
     (remote-path . "~/courses/cs131")
     (projectile-project-name . "Syntax II")
     (org-noter-default-notes-file-names "ling103.org")
     (port . "22")
     (remote-path "~/courses/cs131")
     (user . "classbin")
     (host . "lnxsrv07seas.ucla.edu")
     (org-ref-default-bibliography "~/Dropbox/Documents/Courses/ling120c/ling120c.bib")))
 '(scimax-dir "/Users/jacobchaffin/.emacs.d/etc/local/scimax/" t)
 '(scimax-snippet-dir "/Users/jacobchaffin/.emacs.d/etc/yasnippet/snippets/")
 '(skeletor-completing-read-function 'ivy-completing-read)
 '(skeletor-project-directory "/Users/jacobchaffin/Developer/Projects/")
 '(skeletor-python-bin-search-path '("/usr/local/bin" "/usr/bin"))
 '(skeletor-user-directory "/Users/jacobchaffin/.emacs.d/etc/skeletons/")
 '(tramp-remote-path
   '("/usr/local/cs/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
 '(treemacs-collapse-dirs 3)
 '(treemacs-deferred-git-apply-delay 0.5)
 '(treemacs-display-in-side-window t)
 '(treemacs-file-event-delay 5000)
 '(treemacs-file-follow-delay 0.2)
 '(treemacs-follow-after-init t)
 '(treemacs-follow-recenter-distance 0.1)
 '(treemacs-goto-tag-strategy 'refetch-index)
 '(treemacs-indentation 2)
 '(treemacs-indentation-string " ")
 '(treemacs-is-never-other-window nil)
 '(treemacs-max-git-entries 5000)
 '(treemacs-no-png-images nil)
 '(treemacs-persist-file "/Users/jacobchaffin/.emacs.d/var/treemacs-persist")
 '(treemacs-project-follow-cleanup nil)
 '(treemacs-recenter-after-file-follow nil)
 '(treemacs-recenter-after-tag-follow nil)
 '(treemacs-show-cursor nil t)
 '(treemacs-show-hidden-files t)
 '(treemacs-silent-filewatch nil)
 '(treemacs-silent-refresh nil)
 '(treemacs-sorting 'alphabetic-desc)
 '(treemacs-space-between-root-nodes t)
 '(treemacs-tag-follow-cleanup t)
 '(treemacs-tag-follow-delay 1.5)
 '(treemacs-width 35)
 '(typescript-indent-level 2 t)
 '(wakatime-cli-path
   "/Users/jacobchaffin/.local/lib/python3.6/site-packages/wakatime/cli.py")
 '(wakatime-python-bin "/Users/jacobchaffin/.pyenv/shims/python")
 '(which-key-allow-multiple-replacements t)
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
 '(cfw:face-grid ((t (:foreground "DarkGrey" :family "Sans"))))
 '(cfw:face-header ((t (:foreground "#EFEFEF" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t (:background "grey10" :foreground "#979798" :weight bold))))
 '(cfw:face-select ((t (:inherit highlight))))
 '(cfw:face-sunday ((t (:background "grey10" :foreground "#979798" :weight bold))))
 '(cfw:face-title ((t (:inherit variable-pitch :foreground "#DDDDDD" :weight bold :height 2.0))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t (:background "#F33E35" :foreground "gray100" :weight bold))))
 '(cfw:face-toolbar ((t nil)))
 '(cfw:face-toolbar-button-off ((t (:inherit cfw:face-toolbar :background "#404144" :foreground "#EAEBED" :weight bold))))
 '(cfw:face-toolbar-button-on ((t (:background "#F33E35" :weight bold))))
 '(eshell-prompt ((t (:foreground "gray83"))))
 '(lsp-face-highlight-write ((t nil)))
 '(org-outline-numbering-face ((t (:family "Sans" :weight book :inherit (default))))))


