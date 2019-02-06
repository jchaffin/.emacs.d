(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fill-break-at-separators nil)
 '(LaTeX-item-indent nil)
 '(TeX-auto-save t)
 '(TeX-command-extra-options "-shell-escape")
 '(TeX-error-overview-open-after-TeX-run t)
 '(TeX-interactive-mode nil t)
 '(TeX-parse-self t)
 '(TeX-syntactic-comment t t)
 '(abbrev-file-name "/Users/jacobchaffin/.emacs.d/etc/abbrev/defs.el")
 '(add-to-list 'ffip-prune-patterns t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white] t)
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
 '(bibtex-completion-bibliography
   '("/Users/jacobchaffin/Dropbox/org/ref/references.bib" "/Users/jacobchaffin/Dropbox/courses/comsci131/comsci131.bib" "/Users/jacobchaffin/Dropbox/courses/comsci161/comsci161.bib" "/Users/jacobchaffin/Dropbox/courses/ling102/ling102.bib" "/Users/jacobchaffin/Dropbox/courses/ling165c/ling165c.bib"))
 '(bibtex-completion-library-path
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs" "/Users/jacobchaffin/Dropbox/courses/comsci131/materials/pdfs" "/Users/jacobchaffin/Dropbox/courses/comsci161/materials/pdfs" "/Users/jacobchaffin/Dropbox/courses/ling102/materials/pdfs" "/Users/jacobchaffin/Dropbox/courses/ling165c/materials/pdfs"))
 '(bibtex-completion-notes-path "/Users/jacobchaffin/Dropbox/org/ref/notes.org")
 '(bibtex-completion-pdf-file "File" t)
 '(browse-url-chromium-program "/Applications/Chromium.app/Contents/MacOS/Chromium")
 '(cdlatex-insert-auto-labels-in-env-templates t)
 '(cfw:display-calendar-holidays nil)
 '(cfw:fchar-horizontal-line 9473)
 '(cfw:fchar-junction 9547)
 '(cfw:fchar-left-junction 9507)
 '(cfw:fchar-right-junction 9515)
 '(cfw:fchar-top-junction 9519)
 '(cfw:fchar-top-left-corner 9487)
 '(cfw:fchar-top-right-corner 9491)
 '(cfw:fchar-vertical-line 9475)
 '(cfw:org-capture-template
   '("c" "calfw2org" entry
     (file "agenda/schedule.org")
     "*  %?
 %(cfw:org-capture-day)"))
 '(cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap t)
 '(cider-repl-history-size 3000 t)
 '(cider-repl-result-prefix ";; => " t)
 '(cider-repl-use-clojure-font-lock t t)
 '(cider-repl-wrap-history t t)
 '(cider-show-error-buffer nil t)
 '(clang-format-style-option "google" t)
 '(clojure-indent-style :always-indent t)
 '(code-library-directory "/Users/jacobchaffin/.emacs.d/etc/codelibrary" t)
 '(code-library-sync-to-gist t t)
 '(company-box-enable-icon nil t)
 '(company-idle-delay 0.2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(counsel-gtags-auto-update t t)
 '(counsel-gtags-ignore-case t t)
 '(counsel-osx-app-location '("/Applications" "/Applications/Setapp") t)
 '(css-indent-offset 2 t)
 '(deft-auto-save-interval 0 t)
 '(deft-default-extension "org" t)
 '(deft-directory "~/Dropbox/org/notes/" t)
 '(deft-extensions '("org") t)
 '(deft-use-filename-as-title t t)
 '(diary-file "/Users/jacobchaffin/Dropbox/org/diary.org")
 '(dired-dwim-target t)
 '(dired-sidebar-should-follow-file nil t)
 '(dired-sidebar-theme 'none t)
 '(ediff-diff-options "-w")
 '(elisp-format-column 80 t)
 '(enable-recursive-minibuffers t)
 '(eshell-buffer-maximum-lines 20000 t)
 '(eshell-buffer-shorthand t t)
 '(eshell-cmpl-cycle-completions nil t)
 '(eshell-highlight-prompt t t)
 '(eshell-hist-ignoredups t t)
 '(eshell-history-size 350 t)
 '(eshell-plain-echo-behavior t t)
 '(eshell-prompt-regexp "λ " t)
 '(ffip-use-rust-fd t t)
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes nil)
 '(ggtags-highlight-tag nil t)
 '(git-gutter+-disabled-modes '(image-mode org-mode))
 '(ivy-initial-inputs-alist nil t)
 '(ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy-prescient-re-builder)) t)
 '(ivy-sort-max-size 50000)
 '(ivy-todo-file "/Users/jacobchaffin/Dropbox/org/agenda/ivy-todo.org" t)
 '(ivy-use-selectable-prompt nil)
 '(ivy-use-virtual-buffers t)
 '(javadoc-lookup-completing-read-function 'ivy-completing-read t)
 '(js-indent-level 2)
 '(langtool-disabled-rules '("DASH_RULE"))
 '(langtool-language-tool-jar
   "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
 '(langtool-mother-tongue "en")
 '(latex/no-fill-environments
   '("align" "align*" "forest" "forest*" "equation" "equation*" "exe" "tabular" "tikzpicture"))
 '(livedown-open nil t)
 '(lsp-eldoc-render-all nil)
 '(lsp-highlight-symbol-at-point nil t)
 '(lsp-java-compilation-guess-arguments t t)
 '(lsp-java-server-install-dir "/Users/jacobchaffin/.emacs.d/var/eclipse.jdt.ls/server/" t)
 '(lsp-prefer-flymake nil)
 '(lsp-ui-sideline-update-mode 'point)
 '(magic-latex-enable-block-align nil)
 '(magic-latex-enable-block-highlight t)
 '(magic-latex-enable-inline-image nil)
 '(magic-latex-enable-pretty-symbols t)
 '(magic-latex-enable-suscript nil)
 '(magit-completing-read-function 'ivy-completing-read)
 '(magit-save-repository-buffers 'dontask)
 '(markdown-command "multimarkdown")
 '(markdown-open-command 'markdown-open-preview)
 '(mc/always-run-for-all t)
 '(merlin-command 'opam)
 '(merlin-error-after-save nil)
 '(monky-process-type 'cmdserver t)
 '(no-littering-etc-directory "/Users/jacobchaffin/.emacs.d/etc" t)
 '(nrepl-hide-special-buffers t t)
 '(org-agenda-files
   '("/Users/jacobchaffin/Dropbox/org/agenda/schedule.org" "/Users/jacobchaffin/Dropbox/Documents/2018/fall/ling165b/TODOs.org" "/Users/jacobchaffin/Dropbox/Documents/2018/fall/ling103/TODOs.org" "/Users/jacobchaffin/Dropbox/courses/comsci161/comsci161.org" "/Users/jacobchaffin/dotfiles/latex/orgling/orgling.org" "/Users/jacobchaffin/Dropbox/Documents/2018/fall/cs131/TODOs.org" "/Users/jacobchaffin/Dropbox/courses/comsci131/comsci131.org" "/Users/jacobchaffin/Dropbox/courses/ling102/ling102.org"))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-time-leading-zero t)
 '(org-annotate-file-storage-file "~/Dropbox/org/annotate.org" t)
 '(org-attach-auto-tag "attach")
 '(org-babel-load-languages
   '((C . t)
     (clojure . t)
     (clojurescript . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (lisp . t)
     (emacs-lisp . t)
     (http . t)
     (perl . t)
     (python . t)
     (plantuml . t)
     (java . t)
     (ruby . t)
     (R . t)
     (shell . t)
     (org . t)))
 '(org-babel-uppercase-example-markers t nil nil "Customized with use-package org")
 '(org-bullets-bullet-list '("‣" "•"))
 '(org-catch-invisible-edits t)
 '(org-confirm-babel-evaluate nil nil nil "Customized with use-package org")
 '(org-default-notes-file "/Users/jacobchaffin/Dropbox/org/notes.org")
 '(org-edit-src-persistent-message nil)
 '(org-entities-user
   '(("vdots" "\\vdots{}" t "&x2999" "..." "..." "⁞")
     ("alpha" "\\alpha" t "&alpha;" "alpha" "alpha" "𝛼")
     ("beta" "\\beta" t "&beta;" "beta" "beta" "𝛽")
     ("gamma" "\\gamma" t "&gamma;" "gamma" "gamma" "𝛾")
     ("in" "\\in" t "&isin;" "[element of]" "[element of]" "∈")
     ("vb" "\\vb" t "" "" "" "|")
     ("llbracket" "\\llbracket" t "" "" "" "⟦")
     ("rrbracket" "\\rrbracket" t "" "" "" "⟧")
     ("langle" "\\langle" t "" "" "" "⟨")
     ("rangle" "\\rangle" t "" "" "" "⟩")))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-gcal-file-alist
   '(("jchaffin@g.ucla.edu" . "/Users/jacobchaffin/Dropbox/org/agenda/schedule.org")))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-journal-date-format "%A, %B %d %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "/Users/jacobchaffin/Dropbox/org/journal")
 '(org-journal-enable-agenda-integration t)
 '(org-journal-time-format "")
 '(org-journal-time-prefix "* ")
 '(org-list-allow-alphabetical t)
 '(org-onenote-token-file
   "/Users/jacobchaffin/.emacs.d/var/org/onenote-oauth2.plstore" t)
 '(org-pandoc-options '((standalone . t)))
 '(org-pretty-entities t)
 '(org-preview-latex-image-directory "~/.ltximg")
 '(org-protocol-default-template-key "l")
 '(org-ref-bibliography-notes "/Users/jacobchaffin/Dropbox/org/ref/notes.org")
 '(org-ref-bibtex-hydra-key-binding "j")
 '(org-ref-completion-library 'org-ref-ivy-cite)
 '(org-ref-default-bibliography '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(org-ref-notes-directory "/Users/jacobchaffin/Dropbox/org/ref/notes")
 '(org-ref-pdf-directory "/Users/jacobchaffin/Dropbox/Documents/pdfs/")
 '(org-ref-show-broken-links t)
 '(org-ref-show-citation-on-enter t)
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'reorganize-frame)
 '(org-startup-indented t)
 '(org-tags-column 0)
 '(org-todo-keyword-faces '(("CANCELLED" :foreground "yellow")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "|" "CANCELLED(c)")))
 '(org-use-sub-superscripts '{})
 '(org-velocity-bucket "/Users/jacobchaffin/Dropbox/org/bucket.org")
 '(parinfer-extensions '(defaults smart-yank pretty-parens paredit) t)
 '(pcomplete-cycle-completions nil)
 '(pretty-outlines-ellipsis "" t)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-switch-project-action 'projectile-dired)
 '(safe-local-variable-values
   '((projectile-project-name . "Syntax II")
     (org-ref-pdf-directory . "/Users/jacobchaffin/Dropbox/courses/ling165b/materials/readings/")
     (org-ref-default-citation-link . "citep")))
 '(scimax-dir "/Users/jacobchaffin/.emacs.d/etc/local/scimax" t)
 '(scimax-snippet-dir "/Users/jacobchaffin/.emacs.d/etc/yasnippet/snippets/")
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(shell-pop-full-span t t)
 '(shell-pop-term-shell "/usr/local/bin/zsh" t)
 '(shell-pop-window-position 'bottom t)
 '(shell-pop-window-size 30 t)
 '(skeletor-completing-read-function 'ivy-completing-read)
 '(skeletor-project-directory "/Users/jacobchaffin/Developer/Projects")
 '(skeletor-python-bin-search-path '("/usr/local/bin" "/usr/bin"))
 '(skeletor-user-directory
   "/Users/jacobchaffin/.emacs.d/etc/skeletor/project-skeletons")
 '(sp-highlight-pair-overlay nil t)
 '(sp-highlight-wrap-overlay nil t)
 '(sp-highlight-wrap-tag-overlay nil t)
 '(sp-show-pair-delay 0.2 t)
 '(sp-show-pair-from-inside t t)
 '(tramp-default-method "ssh")
 '(tramp-default-user "jacobchaffin")
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
 '(treemacs-python-executable "/usr/local/bin/python3")
 '(treemacs-recenter-after-file-follow nil)
 '(treemacs-recenter-after-tag-follow nil)
 '(treemacs-show-cursor nil)
 '(treemacs-show-hidden-files t)
 '(treemacs-silent-filewatch nil)
 '(treemacs-silent-refresh nil)
 '(treemacs-sorting 'alphabetic-desc)
 '(treemacs-space-between-root-nodes t)
 '(treemacs-tag-follow-cleanup t)
 '(treemacs-tag-follow-delay 1.5)
 '(treemacs-width 35)
 '(typescript-indent-level 2 t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(visual-fill-column-width 86)
 '(wakatime-cli-path "/usr/local/lib/python3.7/site-packages/wakatime/cli.py")
 '(wakatime-python-bin "/usr/local/Cellar/python/3.7.2_1/bin/python3")
 '(web-mode-block-padding 0 t)
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-comment-style 2 t)
 '(web-mode-enable-auto-pairing t t)
 '(web-mode-enable-block-face t t)
 '(web-mode-enable-comment-keywords t t)
 '(web-mode-enable-css-colorization t t)
 '(web-mode-enable-current-column-highlight t t)
 '(web-mode-enable-current-element-highlight t t)
 '(web-mode-enable-heredoc-fontification t t)
 '(web-mode-enable-part-face t t)
 '(web-mode-engines-alist '(("php" . "\\.phtml\\'") ("blade" . "\\.blade\\'")) t)
 '(web-mode-markup-indent-offset 2 t)
 '(web-mode-script-padding 1 t)
 '(web-mode-style-padding 1 t)
 '(which-key-allow-multiple-replacements t)
 '(which-key-compute-remaps t)
 '(which-key-enable-extended-define-key t)
 '(which-key-separator " → "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "gray83"))))
 '(org-indent-face (((:inherit (org-hide)))))
 '(org-outline-numbering-face ((t (:family "Sans" :weight book :inherit (default)))))
 '(sp-show-pair-match-face ((t (:underline (:color foreground-color :style line) :inherit (show-paren-match))))))
