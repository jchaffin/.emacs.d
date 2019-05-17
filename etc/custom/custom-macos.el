;;; custom-macos.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fill-break-at-separators nil)
 '(LaTeX-item-indent nil)
 '(TeX-auto-save t)
 '(TeX-command-default "LatexMk" t)
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
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
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
 '(bibtex-align-at-equal-sign t)
 '(bibtex-autokey-name-year-separator "-")
 '(bibtex-autokey-titleword-length 5)
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-titlewords-stretch 1)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "-")
 '(bibtex-completion-additional-search-fields '(tags))
 '(bibtex-completion-bibliography '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(bibtex-completion-library-path
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs/ai" "/Users/jacobchaffin/Dropbox/Documents/pdfs/algorithms" "/Users/jacobchaffin/Dropbox/Documents/pdfs/clojure" "/Users/jacobchaffin/Dropbox/Documents/pdfs/comsci" "/Users/jacobchaffin/Dropbox/Documents/pdfs/history" "/Users/jacobchaffin/Dropbox/Documents/pdfs/javascript" "/Users/jacobchaffin/Dropbox/Documents/pdfs/latex" "/Users/jacobchaffin/Dropbox/Documents/pdfs/lisp" "/Users/jacobchaffin/Dropbox/Documents/pdfs/manuals" "/Users/jacobchaffin/Dropbox/Documents/pdfs/papers" "/Users/jacobchaffin/Dropbox/Documents/pdfs/phonetics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/python" "/Users/jacobchaffin/Dropbox/Documents/pdfs/refcards" "/Users/jacobchaffin/Dropbox/Documents/pdfs/semantics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/syntax"))
 '(bibtex-completion-notes-path "/Users/jacobchaffin/Dropbox/org/ref/notes.org")
 '(bibtex-completion-notes-template-one-file
   "** ${title} (${year}):
 :PROPERTIES:
 :Custom_ID: ${=key=}
 :AUTHOR: ${author-or-editor}
 :JOURNAL: ${journal}
 :YEAR: ${year}
 :DOI: ${doi}
 :VOLUME: ${volume}
 :END:

")
 '(bibtex-completion-pdf-symbol "")
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
 '(cider-repl-history-size 3000)
 '(cider-repl-result-prefix ";; => ")
 '(cider-repl-use-clojure-font-lock t)
 '(cider-repl-wrap-history t)
 '(cider-show-error-buffer nil)
 '(clang-format-style-option "google" t)
 '(clojure-indent-style :always-indent)
 '(code-library-directory "/Users/jacobchaffin/.emacs.d/etc/codelibrary" t)
 '(code-library-sync-to-gist t t)
 '(company-box-enable-icon nil t)
 '(company-idle-delay 0.2)
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates nil t)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-transformers nil)
 '(counsel-gtags-auto-update t t)
 '(counsel-gtags-ignore-case t t)
 '(counsel-osx-app-location '("/Applications" "/Applications/Setapp") t)
 '(counsel-projectile-org-capture-templates
   '(("t" "[${name}] Task" entry
      (file "${root}/${name}.org")
      "* ☛ TODO %?
  %u
  %a")
     ("n" "[${name}] Note" entry
      (file+headline "${root}/notes.org" "Notes")
      "* ☛ %a %?
  %u
")))
 '(cquery-executable "/usr/local/bin/cquery" t)
 '(cquery-extra-init-params
   '(:index
     (:comments 2)
     :cacheFormat "msgpack" :completion
     (:detailedLabel t)) t)
 '(css-indent-offset 2)
 '(deft-auto-save-interval 0 t)
 '(deft-default-extension "org" t)
 '(deft-directory "~/Dropbox/org/notes/" t)
 '(deft-extensions '("org") t)
 '(deft-use-filename-as-title t t)
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-sidebar-should-follow-file nil t)
 '(dired-sidebar-theme 'none t)
 '(ebib-extra-fields
   '((BibTeX "crossref" "annote" "abstract" "keywords" "file" "tags" "timestamp" "url" "doi")
     (biblatex "crossref" "annotation" "abstract" "keywords" "file" "tags" "timestamp")))
 '(ebib-file-associations '(("pdf")))
 '(ebib-file-search-dirs
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs/ai" "/Users/jacobchaffin/Dropbox/Documents/pdfs/algorithms" "/Users/jacobchaffin/Dropbox/Documents/pdfs/clojure" "/Users/jacobchaffin/Dropbox/Documents/pdfs/comsci" "/Users/jacobchaffin/Dropbox/Documents/pdfs/history" "/Users/jacobchaffin/Dropbox/Documents/pdfs/javascript" "/Users/jacobchaffin/Dropbox/Documents/pdfs/latex" "/Users/jacobchaffin/Dropbox/Documents/pdfs/lisp" "/Users/jacobchaffin/Dropbox/Documents/pdfs/manuals" "/Users/jacobchaffin/Dropbox/Documents/pdfs/papers" "/Users/jacobchaffin/Dropbox/Documents/pdfs/phonetics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/python" "/Users/jacobchaffin/Dropbox/Documents/pdfs/refcards" "/Users/jacobchaffin/Dropbox/Documents/pdfs/semantics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/syntax"))
 '(ebib-latex-preamble
   "\\documentclass{article} \\usepackage{natbib} \\bibliographystyle{plainnat}")
 '(ebib-preload-bib-files '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(ebib-reading-list-file "/Users/jacobchaffin/Dropbox/org/ref/reading.org")
 '(ediff-diff-options "-w")
 '(elisp-format-column 80 t)
 '(emmms-info-asynchronously nil t)
 '(emms-playlist-buffer-name "*Music*")
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
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes nil)
 '(ggtags-highlight-tag nil t)
 '(git-gutter+-disabled-modes '(image-mode org-mode))
 '(git-timemachine-abbreviation-length 7 t)
 '(golden-ratio-auto-scale t t)
 '(inferior-lisp-program "clisp")
 '(ivy-format-function 'ivy-format-function-line)
 '(ivy-initial-inputs-alist nil)
 '(ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy-prescient-re-builder)) t)
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-sort-max-size 50000)
 '(ivy-use-selectable-prompt nil)
 '(ivy-use-virtual-buffers t)
 '(javadoc-lookup-completing-read-function 'ivy-completing-read)
 '(js-indent-level 2)
 '(langtool-disabled-rules '("DASH_RULE"))
 '(langtool-language-tool-jar
   "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
 '(langtool-mother-tongue "en")
 '(latex/no-fill-environments
   '("align" "align*" "forest" "forest*" "equation" "equation*" "exe" "tabular" "tikzpicture" "prooftree"))
 '(livedown-open nil t)
 '(load-prefer-newer t)
 '(lsp-java-compilation-guess-arguments t t)
 '(lsp-java-server-install-dir "/Users/jacobchaffin/.emacs.d/var/eclipse.jdt.ls/server/")
 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-peek-always-show t)
 '(magic-latex-enable-block-align nil)
 '(magic-latex-enable-block-highlight t)
 '(magic-latex-enable-inline-image nil)
 '(magic-latex-enable-pretty-symbols t)
 '(magic-latex-enable-suscript nil)
 '(magit-completing-read-function 'ivy-completing-read)
 '(magit-save-repository-buffers 'dontask)
 '(mail-user-agent 'mu4e-user-agent)
 '(markdown-command "multimarkdown")
 '(markdown-open-command 'markdown-open-preview)
 '(mc/always-run-for-all t)
 '(merlin-command 'opam)
 '(merlin-error-after-save nil)
 '(midnight-period 7200 t)
 '(monky-process-type 'cmdserver t)
 '(mu4e-alert-interesting-mail-query
   "flag:unread maildir:/school/INBOX OR flag:unread maildir:/personal/INBOX")
 '(mu4e-compose-context-policy 'ask-if-none)
 '(mu4e-compose-format-flowed t)
 '(mu4e-context-policy 'pick-first)
 '(mu4e-get-mail-command "offlineimap -o -q")
 '(mu4e-maildir "~/.mail")
 '(mu4e-sent-messages-behavior 'delete)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(no-littering-etc-directory "/Users/jacobchaffin/.emacs.d/etc" t)
 '(no-littering-var-directory "/Users/jacobchaffin/.emacs.d/var" t)
 '(nrepl-hide-special-buffers t)
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
 '(org-attach-auto-tag "attach")
 '(org-attach-store-link-p t)
 '(org-babel-uppercase-example-markers t)
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-bullets-bullet-list '("▸" "●"))
 '(org-catch-invisible-edits 'smart)
 '(org-clock-persist t)
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-confirm-babel-evaluate nil)
 '(org-contacts-files '("~/Dropbox/org/contacts.org"))
 '(org-ctrl-k-protect-subtree 'error)
 '(org-default-notes-file "/Users/jacobchaffin/Dropbox/org/notes.org")
 '(org-directory "/Users/jacobchaffin/Dropbox/org")
 '(org-display-internal-link-with-indirect-buffer t)
 '(org-ellipsis "")
 '(org-export-with-sub-superscripts '{})
 '(org-gcal-file-alist
   '(("jchaffin@g.ucla.edu" . "/Users/jacobchaffin/Dropbox/org/agenda/schedule.org")))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-id-locations-file "/Users/jacobchaffin/.emacs.d/var/org/id-locations.el")
 '(org-journal-date-format "%A, %B %d %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "/Users/jacobchaffin/Dropbox/org/journal")
 '(org-journal-enable-agenda-integration nil)
 '(org-journal-time-format "")
 '(org-journal-time-prefix "* ")
 '(org-latex-compiler "xelatex")
 '(org-latex-hyperref-template nil)
 '(org-latex-listings 'minted)
 '(org-latex-minted-options
   '(("mathescape" "true")
     ("linenos" "true")
     ("frame" "lines")
     ("framesep" "2mm")))
 '(org-latex-packages-alist '(("" "booktabs" nil)))
 '(org-latex-prefer-user-labels t)
 '(org-link-search-must-match-exact-headline 'query-to-create)
 '(org-list-allow-alphabetical t)
 '(org-modules
   '(org-bbdb org-bibtex org-crypt org-eww org-habit org-id org-info org-inlinetask org-protocol org-tempo org-eshell org-annotate-file org-checklist org-collector org-mac-iCal org-mac-link org-velocity ol-bookmark ol-man ol-elisp-symbol) nil nil "Customized with use-package org")
 '(org-noter-always-create-frame nil t)
 '(org-noter-hide-other nil t)
 '(org-onenote-token-file
   "/Users/jacobchaffin/.emacs.d/var/org/onenote-oauth2.plstore" t)
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
     ("python" . "")
     ("kotlin" . "")
     ("tex" . "")
     ("noexport" . "❌")
     ("ignore" . "🚫")
     ("bib" . "")
     ("TOC_3_gh" . "")
     ("web" . "🔗")
     ("@stackexchange" . "")
     ("@stackoverflow" . "")
     ("@github" . "")
     ("@youtube" . "")
     ("@reddit" . "")
     ("@shopping" . "🛒")
     ("@audiophile" . "")
     ("@gift" . "")
     ("@inbox" . "")
     ("speaker" . "")))
 '(org-projectile-capture-template "* ☛ TODO %?
  %i
  %a" t)
 '(org-projectile-per-project-filepath 'org-projectile-per-project-function t)
 '(org-ref-bibliography-notes "/Users/jacobchaffin/ref/notes.org")
 '(org-ref-completion-library 'org-ref-ivy-cite)
 '(org-ref-default-bibliography '("/Users/jacobchaffin/Dropbox/org/ref/references.bib"))
 '(org-ref-note-title-format
   "** %y - %t
    :PROPERTIES:
    :Custom_ID: %k
    :AUTHOR: %9a
    :JOURNAL: %j
    :YEAR: %y
    :VOLUME: %v
    :PAGES: %p
    :DOI: %D
    :URL: %U
    :END:

")
 '(org-ref-notes-directory "/Users/jacobchaffin/Dropbox/org/ref/notes")
 '(org-ref-pdf-directory
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs/ai" "/Users/jacobchaffin/Dropbox/Documents/pdfs/algorithms" "/Users/jacobchaffin/Dropbox/Documents/pdfs/clojure" "/Users/jacobchaffin/Dropbox/Documents/pdfs/comsci" "/Users/jacobchaffin/Dropbox/Documents/pdfs/history" "/Users/jacobchaffin/Dropbox/Documents/pdfs/javascript" "/Users/jacobchaffin/Dropbox/Documents/pdfs/latex" "/Users/jacobchaffin/Dropbox/Documents/pdfs/lisp" "/Users/jacobchaffin/Dropbox/Documents/pdfs/manuals" "/Users/jacobchaffin/Dropbox/Documents/pdfs/papers" "/Users/jacobchaffin/Dropbox/Documents/pdfs/phonetics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/python" "/Users/jacobchaffin/Dropbox/Documents/pdfs/refcards" "/Users/jacobchaffin/Dropbox/Documents/pdfs/semantics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/syntax"))
 '(org-ref-ref-types
   '("ref" "eqref" "pageref" "nameref" "autoref" "cref" "Cref" "vref"))
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
 '(org-variable-pitch-fixed-font "Fira Code")
 '(org-velocity-bucket "/Users/jacobchaffin/Dropbox/org/notes.org")
 '(org-yank-adjusted-subtrees t)
 '(org-yank-folded-subtrees t)
 '(parinfer-extensions '(defaults smart-yank pretty-parens paredit) t)
 '(pcomplete-cycle-completions nil)
 '(pdf-annot-activate-created-annotations t t)
 '(pdf-view-resize-factor 2)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-switch-project-action 'projectile-dired)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(reftex-default-bibliography "/Users/jacobchaffin/Dropbox/org/ref/references.bib")
 '(reftex-plug-into-AUCTeX '(nil nil t t t))
 '(reftex-use-fonts t)
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
 '(sly-swithc-to-existing-lisp 'always t)
 '(sp-highlight-pair-overlay nil)
 '(sp-highlight-wrap-overlay nil)
 '(sp-highlight-wrap-tag-overlay nil)
 '(sp-show-pair-delay 0.125)
 '(sp-show-pair-from-inside t)
 '(swiper-stay-on-quit t)
 '(tramp-default-method "ssh")
 '(tramp-default-user "classbin")
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
 '(treemacs-python-executable "/Users/jacobchaffin/.pyenv/shims/python3")
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
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(visual-fill-column-width 86)
 '(wakatime-cli-path
   "/Users/jacobchaffin/.local/pipx/venvs/wakatime/lib/python3.7/site-packages/wakatime/cli.py")
 '(wakatime-python-bin "/Users/jacobchaffin/.pyenv/versions/3.7.2/bin/python3.7")
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
 '(aw-leading-char-face ((t (:inverse-video t :inherit avy-lead-face :height 2.0))))
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
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Fira Sans")))))
