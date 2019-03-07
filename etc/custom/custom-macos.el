;;; custom-macos.el --
;;
;; Author: Jacob Chaffin <jchaffin@ucla.edu>
;; Copyright © 2019, Jacob Chaffin, all rights reserved.
;; Created:  1 March 2019
;;
;;; Commentary:
;;
;;
;;
;;; Code:


;;; custom-macos.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fill-break-at-separators nil t)
 '(LaTeX-fontspec-font-list-default
   '("STIX Two Text" "STIX Two Math" "DejaVu Sans Mono" "Times New Roman"))
 '(LaTeX-item-indent nil t)
 '(TeX-auto-save t t)
 '(TeX-error-overview-open-after-TeX-run t t)
 '(TeX-interactive-mode nil t)
 '(TeX-parse-self t t)
 '(TeX-syntactic-comment t t)
 '(abbrev-file-name "/Users/jacobchaffin/.emacs.d/etc/abbrev/defs.el")
 '(add-to-list 'ffip-prune-patterns t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white] t)
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
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
 '(bibtex-autokey-name-year-separator "-")
 '(bibtex-autokey-titleword-length 5)
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-titlewords-stretch 1)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "-")
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
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates nil t)
 '(company-math-allow-latex-symbols-in-faces '(tex-math font-latex-math-face))
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-transformers nil)
 '(counsel-gtags-auto-update t t)
 '(counsel-gtags-ignore-case t t)
 '(counsel-osx-app-location '("/Applications" "/Applications/Setapp") t)
 '(cquery-executable "/usr/local/bin/cquery" t)
 '(cquery-extra-init-params
   '(:index
     (:comments 2)
     :cacheFormat "msgpack" :completion
     (:detailedLabel t)) t)
 '(css-indent-offset 2 t)
 '(deft-auto-save-interval 0 t)
 '(deft-default-extension "org" t)
 '(deft-directory "~/Dropbox/org/notes/" t)
 '(deft-extensions '("org") t)
 '(deft-use-filename-as-title t t)
 '(delete-by-moving-to-trash t)
 '(diary-file "/Users/jacobchaffin/Dropbox/org/diary.org")
 '(dired-dwim-target t)
 '(dired-sidebar-should-follow-file nil t)
 '(dired-sidebar-theme 'none t)
 '(ediff-diff-options "-w" t)
 '(elisp-format-column 80 t)
 '(enable-recursive-minibuffers t)
 '(eshell-buffer-maximum-lines 20000)
 '(eshell-buffer-shorthand t t)
 '(eshell-cmpl-cycle-completions nil t)
 '(eshell-highlight-prompt t t)
 '(eshell-hist-ignoredups t t)
 '(eshell-history-size 350 t)
 '(eshell-plain-echo-behavior t t)
 '(eshell-prompt-regexp "λ " t)
 '(ffip-use-rust-fd t t)
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc) t)
 '(flycheck-emacs-lisp-load-path 'inherit t)
 '(flycheck-global-modes nil t)
 '(ggtags-highlight-tag nil t)
 '(git-gutter+-disabled-modes '(image-mode org-mode))
 '(ivy-initial-inputs-alist nil)
 '(ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy-prescient-re-builder)) t)
 '(ivy-sort-max-size 50000)
 '(ivy-todo-file "/Users/jacobchaffin/Dropbox/org/agenda/ivy-todo.org" t)
 '(ivy-use-selectable-prompt nil)
 '(ivy-use-virtual-buffers t)
 '(javadoc-lookup-completing-read-function 'ivy-completing-read)
 '(js-indent-level 2 t)
 '(langtool-disabled-rules '("DASH_RULE"))
 '(langtool-language-tool-jar
   "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
 '(langtool-mother-tongue "en")
 '(latex/no-fill-environments
   '("align" "align*" "forest" "forest*" "equation" "equation*" "exe" "tabular" "tikzpicture" "prooftree") t)
 '(livedown-open nil t)
 '(load-prefer-newer t)
 '(lsp-eldoc-render-all nil)
 '(lsp-highlight-symbol-at-point nil t)
 '(lsp-java-compilation-guess-arguments t t)
 '(lsp-java-server-install-dir "/Users/jacobchaffin/.emacs.d/var/eclipse.jdt.ls/server/" t)
 '(lsp-prefer-flymake nil)
 '(lsp-ui-sideline-update-mode 'point t)
 '(magic-latex-enable-block-align nil t)
 '(magic-latex-enable-block-highlight t t)
 '(magic-latex-enable-inline-image nil t)
 '(magic-latex-enable-pretty-symbols t t)
 '(magic-latex-enable-suscript nil t)
 '(magit-completing-read-function 'ivy-completing-read)
 '(magit-save-repository-buffers 'dontask)
 '(markdown-command "multimarkdown")
 '(markdown-open-command 'markdown-open-preview)
 '(mc/always-run-for-all t t)
 '(merlin-command 'opam)
 '(merlin-error-after-save nil)
 '(monky-process-type 'cmdserver t)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(no-littering-etc-directory "/Users/jacobchaffin/.emacs.d/etc" t)
 '(no-littering-var-directory "/Users/jacobchaffin/.emacs.d/var" t)
 '(nrepl-hide-special-buffers t t)
 '(org-agenda-block-separator "─")
 '(org-agenda-category-icon-alist
   '(("global"
      ("")
      nil nil :ascent center)
     ("schedule"
      ("📅")
      nil nil :ascent center)
     ("LING 165C"
      ("⟦ ⟧")
      nil nil :ascent center)
     ("LING 102"
      ("🗣")
      nil nil :ascent center)
     ("COMSCI 161"
      ("♟")
      nil nil :ascent center)
     ("COMSCI 131"
      ("💻")
      nil nil :ascent center)))
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-diary-file "/Users/jacobchaffin/Dropbox/org/diary.org")
 '(org-agenda-inhibit-startup t)
 '(org-agenda-mouse-1-follows-link t)
 '(org-agenda-remove-tags t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-delay-if-deadline 21)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-time-leading-zero t)
 '(org-agenda-todo-ignore-deadlines t)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-todo-ignore-timestamp t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-annotate-file-storage-file "~/Dropbox/org/annotate.org")
 '(org-archive-location "archive/%s_archive::")
 '(org-attach-auto-tag "attach" t)
 '(org-babel-uppercase-example-markers t nil nil "Customized with use-package org")
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-bullets-bullet-list '("▸" "●"))
 '(org-catch-invisible-edits 'show)
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-confirm-babel-evaluate nil nil nil "Customized with use-package org")
 '(org-contacts-files '("~/Dropbox/org/contacts.org"))
 '(org-ctrl-k-protect-subtree 'error)
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-directory "/Users/jacobchaffin/Dropbox/org")
 '(org-edit-src-persistent-message nil)
 '(org-ellipsis "")
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
     ("Langle" "\\Langle" t "" "" "" "⟨")
     ("Rangle" "\\Rangle" t "" "" "" "⟩")
     ("rangle" "\\rangle" t "" "" "" "⟩")
     ("bigtriangleup" "\\bigtriangleup " t "" "" "" "△")
     ("fracslash" "\\fracslash" t "" "" "" "⁄")
     ("divslash" "\\divslash " t "" "" "" "∕")
     ("mathslash" "\\mathslash" t "" "" "" "/")
     ("subseteq" "\\subseteq" t "" "" "" "⊆")
     ("lbrace" "\\lbrace" t "" "" "" "{")
     ("rbrack" "\\rbrack" t "" "" "" "]")
     ("lbrack" "\\lbrack" t "" "" "" "[")
     ("rbrace" "\\rbrace" t "" "" "" "}")
     ("xsol" "\\xsol" t "" "" "" "⧸")))
 '(org-export-with-sub-superscripts '{})
 '(org-fontify-quote-and-verse-blocks t)
 '(org-gcal-file-alist
   '(("jchaffin@g.ucla.edu" . "/Users/jacobchaffin/Dropbox/org/agenda/schedule.org")))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-id-locations-file "/Users/jacobchaffin/.emacs.d/var/org/id-locations.el")
 '(org-id-locations-file-name "/Users/jacobchaffin/.emacs.d/var/org/id-locations.el" t)
 '(org-insert-heading-respect-content t)
 '(org-journal-date-format "%A, %B %d %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "/Users/jacobchaffin/Dropbox/org/journal")
 '(org-journal-enable-agenda-integration nil)
 '(org-journal-time-format "")
 '(org-journal-time-prefix "* ")
 '(org-latex-compiler "xelatex")
 '(org-latex-hyperref-template nil)
 '(org-latex-listings 'minted)
 '(org-latex-listings-langs
   '((emacs-lisp "Lisp")
     (lisp "Lisp")
     (clojure "Lisp")
     (c "C")
     (cc "C++")
     (fortran "fortran")
     (perl "Perl")
     (cperl "Perl")
     (python "Python")
     (ruby "Ruby")
     (html "HTML")
     (xml "XML")
     (tex "TeX")
     (latex "[LaTeX]TeX")
     (shell-script "bash")
     (gnuplot "Gnuplot")
     (ocaml "Caml")
     (caml "Caml")
     (sql "SQL")
     (sqlite "sql")
     (makefile "make")
     (R "r")))
 '(org-latex-minted-options
   '(("mathescape" "true")
     ("linenos" "true")
     ("frame" "lines")
     ("framesep" "2mm")))
 '(org-latex-packages-alist '(("" "booktabs" nil)))
 '(org-link-search-must-match-exact-headline 'query-to-create)
 '(org-list-allow-alphabetical t)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-id org-info org-inlinetask org-irc org-mhe org-protocol org-rmail org-tempo org-w3m org-eshell org-annotate-file org-bookmark org-checklist org-collector org-mac-iCal org-mac-link org-velocity))
 '(org-onenote-token-file
   "/Users/jacobchaffin/.emacs.d/var/org/onenote-oauth2.plstore" t)
 '(org-pandoc-options '((standalone . t)))
 '(org-pretty-entities t)
 '(org-pretty-tags-surrogate-strings
   '(("homework" . "📕️")
     ("note" . "️📓")
     ("exam" . "📜")
     ("quiz" . "📝")
     ("reading" . "📚")
     ("lecture" . "🎓")
     ("attach" . "📎")
     ("@semantics" . "λ")
     ("@phonetics" . "🗣")
     ("@ai" . "♠")
     ("@comsci" . "💻")
     ("java" . "")
     ("prolog" . "")
     ("scheme" . "λ")
     ("ocaml" . "")
     ("noexport" . "❌")
     ("ignore" . "🚫")
     ("bib" . "")
     ("TOC_3_gh" . "")
     ("web" . "🔗")
     ("@shopping" . "🛒")))
 '(org-projectile-per-project-filepath 'org-project-per-project-function t)
 '(org-ref-bibliography-notes "/Users/jacobchaffin/Dropbox/org/ref/notes.org")
 '(org-ref-default-bibliography '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(org-ref-notes-directory "/Users/jacobchaffin/Dropbox/org/ref/notes")
 '(org-ref-pdf-directory "/Users/jacobchaffin/Dropbox/Documents/pdfs/")
 '(org-ref-show-broken-links t)
 '(org-ref-show-citation-on-enter t)
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-sort-agenda-notime-is-late nil)
 '(org-src-fontify-natively t)
 '(org-src-persistent-message nil t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'current-window)
 '(org-sticky-header-heading-star "•" t)
 '(org-tags-column 0)
 '(org-todo-keyword-faces
   '(("✘ CANCELLED" :foreground "yellow")
     (" DONE" . org-done)
     ("☛ TODO" . org-todo)))
 '(org-todo-keywords
   '((sequence "☛ TODO(t)" "|" " DONE(d)")
     (sequence "|" "✘ CANCELLED(c)")))
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts '{})
 '(org-variable-pitch-face "Fira Sans" t)
 '(org-variable-pitch-fixed-font "Fira Code")
 '(org-velocity-bucket "/Users/jacobchaffin/Dropbox/org/notes.org")
 '(org-yank-adjusted-subtrees t)
 '(org-yank-folded-subtrees t)
 '(parinfer-extensions '(defaults smart-yank pretty-parens paredit) t)
 '(pcomplete-cycle-completions nil)
 '(pdf-annot-activate-created-annotations t t)
 '(pdf-view-resize-factor 2)
 '(pop-up-frames nil)
 '(pretty-outlines-ellipsis "" t)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-switch-project-action 'projectile-dired)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
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
 '(sly-contribs '(sly-fancy) t)
 '(sp-highlight-pair-overlay nil t)
 '(sp-highlight-wrap-overlay nil t)
 '(sp-highlight-wrap-tag-overlay nil t)
 '(sp-show-pair-delay 0.2 t)
 '(sp-show-pair-from-inside t t)
 '(tramp-default-method "ssh")
 '(tramp-default-user "classbin" nil nil "Customized with use-package tramp")
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
 '(wakatime-python-bin "/usr/local/bin/python3")
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
 '(cfw:face-day-title ((t (:weight bold))))
 '(cfw:face-grid ((t (:foreground "DarkGrey"))))
 '(cfw:face-header ((t (:weight bold :foreground "red"))))
 '(cfw:face-holiday ((t (:weight bold :background "#51481F"))))
 '(cfw:face-select ((t (:background "#0195FF"))))
 '(cfw:face-sunday ((t (:inherit (cfw:face-saturday)))))
 '(cfw:face-title ((t (:height 2.0 :weight bold :inherit (variable-pitch)))))
 '(cfw:face-today ((t (:weight bold :background "#182633"))))
 '(cfw:face-toolbar ((t (:background "#3B3B3B"))))
 '(cfw:face-toolbar-button-off ((t (:weight bold :background "Gray10"))))
 '(cfw:face-toolbar-button-on ((t (:background "red"))))
 '(fixed-pitch ((t (:family "Fira Code"))))
 '(org-agenda-calendar-event ((t (:foreground "#cc9393" :inherit (default)))))
 '(org-agenda-date ((t (:underline nil :foreground "#57c7ff" :inherit (variable-pitch)))))
 '(org-variable-pitch-indent-face ((t (:inherit (org-hide fixed-pitch)))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Fira Sans")))))
