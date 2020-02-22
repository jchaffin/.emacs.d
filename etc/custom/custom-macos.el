;;; custom-macos.el ---  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020, Jacob Chaffin, all rights reserved.

;; Version: 0.0.1
;; Author: Jacob Chaffin -- <jchaffin@ucla.edu>
;; URL: https://github.com/jchaffin/custom
;; Created:  1 February 2020
;; Keywords:
;; Package-Requires ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;;
;;
;;
;;; Code:



(provide 'custom-macos)
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
      (("author")
       ("editor")
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
      (("author")
       ("editor")
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
      (("type" "Type of the PhD. thesis")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("MastersThesis" "Master's Thesis"
      (("author")
       ("title" "Title of the master's thesis (BibTeX converts it to lowercase)")
       ("school" "School where the master's thesis was written")
       ("year"))
      (("type" "Type of the master's thesis (if other than \"Master's thesis\")")
       ("address" "Address of the school (if not part of field \"school\") or country")
       ("month")
       ("note")))
     ("TechReport" "Technical Report"
      (("author")
       ("title" "Title of the technical report (BibTeX converts it to lowercase)")
       ("institution" "Sponsoring institution of the report")
       ("year"))
      (("type" "Type of the report (if other than \"technical report\")")
       ("number" "Number of the technical report")
       ("address")
       ("month")
       ("note")))
     ("Manual" "Technical Manual"
      (("title" "Title of the manual"))
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
      (("month")
       ("year")))
     ("Misc" "Miscellaneous"
      (("author")
       ("title" "Title of the work (BibTeX converts it to lowercase)")
       ("howpublished" "The way in which the work was published")
       ("month")
       ("year")
       ("note")))
     ("Online" "Online Resource"
      (("author")
       ("editor")
       ("title")
       ("year")
       ("date")
       ("url"))
      (("subtitle")
       ("titleaddon")
       ("language")
       ("howpublished")
       ("type")
       ("version")
       ("note")
       ("organization")
       ("location")
       ("date")
       ("month")
       ("year")
       ("addendum")
       ("pubstate")
       ("doi")
       ("eprint")
       ("eprintclass")
       ("eprinttype")
       ("url")
       ("urldate")))))
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
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs/ai" "/Users/jacobchaffin/Dropbox/Documents/pdfs/algorithms" "/Users/jacobchaffin/Dropbox/Documents/pdfs/clojure" "/Users/jacobchaffin/Dropbox/Documents/pdfs/comsci" "/Users/jacobchaffin/Dropbox/Documents/pdfs/csling" "/Users/jacobchaffin/Dropbox/Documents/pdfs/history" "/Users/jacobchaffin/Dropbox/Documents/pdfs/home-improv" "/Users/jacobchaffin/Dropbox/Documents/pdfs/javascript" "/Users/jacobchaffin/Dropbox/Documents/pdfs/latex" "/Users/jacobchaffin/Dropbox/Documents/pdfs/lisp" "/Users/jacobchaffin/Dropbox/Documents/pdfs/manuals" "/Users/jacobchaffin/Dropbox/Documents/pdfs/medical" "/Users/jacobchaffin/Dropbox/Documents/pdfs/ocaml" "/Users/jacobchaffin/Dropbox/Documents/pdfs/papers" "/Users/jacobchaffin/Dropbox/Documents/pdfs/phonetics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/python" "/Users/jacobchaffin/Dropbox/Documents/pdfs/refcards" "/Users/jacobchaffin/Dropbox/Documents/pdfs/scheme" "/Users/jacobchaffin/Dropbox/Documents/pdfs/semantics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/syntax"))
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
 '(bibtex-parse-keys-fast nil)
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
 '(codesearch-global-csearchindex "/Users/jacobchaffin/.emacs.d/var/codesearch/index" t)
 '(company-box-enable-icon nil t)
 '(company-idle-delay 0.2)
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates nil t)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-transformers nil)
 '(counsel-bookmark-avoid-dired t)
 '(counsel-describe-function-function 'helpful-callable)
 '(counsel-describe-variable-function 'helpful-variable)
 '(counsel-git-grep-cmd "git --no-pager grep -n --no-color -I -e \"%s\"" t)
 '(counsel-grep-swiper-limit 250000)
 '(counsel-gtags-auto-update t)
 '(counsel-gtags-ignore-case t)
 '(counsel-mode-override-describe-bindings t)
 '(counsel-org-clock-history-include-archives t)
 '(counsel-osx-app-location '("/Applications" "/Applications/Setapp") t)
 '(cquery-executable "/usr/local/bin/cquery" t)
 '(cquery-extra-init-params
   '(:index
     (:comments 2)
     :cacheFormat "msgpack" :completion
     (:detailedLabel t)) t)
 '(css-indent-offset 2 t)
 '(dash-docs-browser-func 'browse-url t)
 '(deft-auto-save-interval 0 t)
 '(deft-default-extension "org" t)
 '(deft-directory "~/Dropbox/org/notes/" t)
 '(deft-extensions '("org") t)
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-sidebar-should-follow-file nil t)
 '(dired-sidebar-theme 'none t)
 '(ebib-extra-fields
   '((BibTeX "crossref" "annote" "abstract" "keywords" "file" "tags" "timestamp" "url" "doi")
     (biblatex "crossref" "annotation" "abstract" "keywords" "file" "tags" "timestamp")) t)
 '(ebib-file-associations '(("pdf")) t)
 '(ebib-file-search-dirs
   '("/Users/jacobchaffin/Dropbox/Documents/pdfs/ai" "/Users/jacobchaffin/Dropbox/Documents/pdfs/algorithms" "/Users/jacobchaffin/Dropbox/Documents/pdfs/clojure" "/Users/jacobchaffin/Dropbox/Documents/pdfs/comsci" "/Users/jacobchaffin/Dropbox/Documents/pdfs/csling" "/Users/jacobchaffin/Dropbox/Documents/pdfs/history" "/Users/jacobchaffin/Dropbox/Documents/pdfs/home-improv" "/Users/jacobchaffin/Dropbox/Documents/pdfs/javascript" "/Users/jacobchaffin/Dropbox/Documents/pdfs/latex" "/Users/jacobchaffin/Dropbox/Documents/pdfs/lisp" "/Users/jacobchaffin/Dropbox/Documents/pdfs/manuals" "/Users/jacobchaffin/Dropbox/Documents/pdfs/medical" "/Users/jacobchaffin/Dropbox/Documents/pdfs/ocaml" "/Users/jacobchaffin/Dropbox/Documents/pdfs/papers" "/Users/jacobchaffin/Dropbox/Documents/pdfs/phonetics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/python" "/Users/jacobchaffin/Dropbox/Documents/pdfs/refcards" "/Users/jacobchaffin/Dropbox/Documents/pdfs/scheme" "/Users/jacobchaffin/Dropbox/Documents/pdfs/semantics" "/Users/jacobchaffin/Dropbox/Documents/pdfs/syntax") t)
 '(ebib-latex-preamble
   "\\documentclass{article} \\usepackage{natbib} \\bibliographystyle{plainnat}" t)
 '(ebib-preload-bib-files '("/Users/jacobchaffin/Dropbox/org/ref/references.bib") t)
 '(ebib-reading-list-file "/Users/jacobchaffin/Dropbox/org/ref/reading.org" t)
 '(ediff-diff-options "-w" t)
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
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc) t)
 '(flycheck-emacs-lisp-load-path 'inherit t)
 '(flycheck-global-modes nil t)
 '(geiser-scheme-implementation 'racket t)
 '(ggtags-highlight-tag nil t)
 '(git-gutter+-disabled-modes '(image-mode org-mode))
 '(git-timemachine-abbreviation-length 7 t)
 '(golden-ratio-auto-scale t t)
 '(grip-binary-path "/Users/jacobchaffin/.local/bin/grip" t)
 '(grip-preview-use-webkit t t)
 '(haskell-process-path-ghci "/Users/jacobchaffin/.ghcup/bin/ghci")
 '(inferior-lisp-program "clisp" t)
 '(ivy-format-function 'ivy-format-function-line t)
 '(ivy-initial-inputs-alist nil)
 '(ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy-prescient-re-builder)) t)
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-sort-max-size 50000)
 '(ivy-use-selectable-prompt nil)
 '(ivy-use-virtual-buffers t)
 '(javadoc-lookup-completing-read-function 'ivy-completing-read t)
 '(js-indent-level 2 t)
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
 '(lsp-ui-doc-header t t)
 '(lsp-ui-doc-position 'at-point t)
 '(lsp-ui-doc-use-webkit t t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-peek-always-show t t)
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
 '(no-littering-etc-directory "/Users/jacobchaffin/.emacs.d/etc" t)
 '(no-littering-var-directory "/Users/jacobchaffin/.emacs.d/var" t)
 '(nrepl-hide-special-buffers t t)
 '(org-agenda-block-separator "─")
 '(org-agenda-category-icon-alist
   '(("global"
      ("🌍")
      nil nil :ascent center)
     ("schedule"
      ("📅")
      nil nil :ascent center)))
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-custom-commands '(("c" "Simple agenda view" ((agenda "") (alltodo "")))))
 '(org-agenda-diary-file "/Users/jacobchaffin/Dropbox/org/diary.org")
 '(org-agenda-files
   '("~/.emacs.d/dotemacs.org" "/Users/jacobchaffin/Dropbox/org/notes.org" "/Users/jacobchaffin/Dropbox/org/agenda/schedule.org" "/Users/jacobchaffin/Dropbox/org/agenda/TODOS.org" "/Users/jacobchaffin/Dropbox/courses/ling185a/ling185a.org" "/Users/jacobchaffin/Dropbox/courses/ling102/ling102.org" "/Users/jacobchaffin/Dropbox/Documents/2019/fall/comsci131/comsci131.org" "/Users/jacobchaffin/Dropbox/Documents/2019/winter/ling165c/ling165c.org"))
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
 '(org-annotate-file-storage-file "/Users/jacobchaffin/Dropbox/org/annotate.org")
 '(org-archive-location "archive/%s_archive::")
 '(org-attach-auto-tag "attach" t)
 '(org-attach-store-link-p t t)
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
 '(org-ellipsis "")
 '(org-entities-user
   '(("alpha" "\\alpha" t "" "" "" "𝛼")
     ("beta" "\\beta" t "" "" "" "𝛽")
     ("gamma" "\\gamma" t "" "" "" "𝛾")
     ("lambda" "\\lambda" t "" "" "" "λ")
     ("cat" "\\cat" t "" "" "" "/")
     ("lbrack" "\\lbrack" t "" "" "" "[")
     ("rbrack" "\\rbrack" t "" "" "" "]")
     ("lbrace" "\\lbrace" t "" "" "" "{")
     ("rbrace" "\\rbrace" t "" "" "" "}")
     ("Rangle" "\\Rangle" t "" "" "" "⟩")
     ("Langle" "\\Langle" t "" "" "" "⟨")
     ("lBrack" "\\lBrack" t "" "" "" "⟦")
     ("rBrack" "\\rBrack" t "" "" "" "⟧")
     ("text" "\\text" t "" "" "" "T")
     ("vb" "\\vb" t "" "" "" "|")
     ("xsol" "\\xsol" t "" "" "" "⧸")
     ("textbackslash" "\\textbackslash" t "" "" "" "＼")
     ("mathslash" "\\mathslash" t "" "" "" "/")
     ("rfrac" "\\rfrac" t "" "" "" "/")
     ("subseteq" "\\subseteq" t "" "" "" "⊆")
     ("subsetneq" "\\subsetneq" t "" "" "" "⊊")
     ("textdollar" "\\textdollar" t "" "" "" "＄")
     ("Longrightarrow" "\\Longrightarrow" t "" "" "" "⟹")))
 '(org-export-with-sub-superscripts '{})
 '(org-gcal-file-alist
   '(("jchaffin@g.ucla.edu" . "/Users/jacobchaffin/Dropbox/org/agenda/schedule.org")))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id 'create-if-interactive)
 '(org-id-locations-file "/Users/jacobchaffin/.emacs.d/var/org/id-locations.el")
 '(org-insert-heading-respect-content t)
 '(org-journal-date-format "%A, %B %d %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "/Users/jacobchaffin/Dropbox/org/journal")
 '(org-journal-enable-agenda-integration nil)
 '(org-journal-time-format "")
 '(org-journal-time-prefix "* ")
 '(org-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article-standalone" "\\documentclass{article}
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*a{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("uclaling" "\\documentclass{uclaling}
      [NO-DEFAULT-PACKAGES]
      [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("uclacs" "\\documentclass{uclacs}
      [NO-DEFAULT-PACKAGES]
      [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("humanities" "\\documentclass{humanities}
      [NO-DEFAULT-PACKAGES]
      [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("unicode-math" "\\documentclass{article}
     [PACKAGES]
     [NO-DEFAULT-PACKAGES]
     [EXTRA]
	   \\usepackage{fontspec}
	   \\usepackage{amsmath}
	   \\usepackage{xltxtra}
	   \\usepackage{unicode-math}
	   \\setmathfont{STIX2Math}[
	     Path/Users/jacobchaffin/Library/Fonts/,
	     Extension={.otf},
	     Scale=1]
	   \\setmainfont{STIX2Text}[
	     Path/Users/jacobchaffin/Library/Fonts/,
	     Extension={.otf},
	     UprightFont={*-Regular},
	     BoldFont={*-Bold},
	     ItalicFont={*-Italic},
	     BoldItalicFont={*-BoldItalic}]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
 '(org-latex-compiler "xelatex")
 '(org-latex-hyperref-template nil)
 '(org-latex-listings 'minted)
 '(org-latex-minted-options
   '(("mathescape" "true")
     ("escapeinside" "@@")
     ("breaklines" "true")
     ("fontsize" "\\footnotesize")))
 '(org-latex-prefer-user-labels t)
 '(org-link-search-must-match-exact-headline 'query-to-create)
 '(org-link-use-indirect-buffer-for-internals t)
 '(org-list-allow-alphabetical t)
 '(org-modules
   '(org-bbdb org-bibtex org-crypt org-eww org-habit org-id org-info org-inlinetask org-protocol org-tempo org-eshell org-annotate-file org-checklist org-collector org-mac-iCal org-mac-link org-velocity ol-bookmark ol-man ol-elisp-symbol))
 '(org-noter-always-create-frame nil t)
 '(org-noter-hide-other nil t)
 '(org-onenote-token-file
   "/Users/jacobchaffin/.emacs.d/var/org/onenote-oauth2.plstore" t)
 '(org-pandoc-options '((standalone . t)))
 '(org-pretty-tags-surrogate-strings
   '(("homework" . "📕️")
("note" . "️📓")
("assignment" . "📒️")
("exam" . "📝")
("quiz" . "📘")
("reading" . "📚")
("lecture" . "🎓")
("discussion" . "")
("@semantics" . "λ")
("@phonetics" . "🗣")
("@ai" . "♘")
("@comsci" . "💻")
("@astro" . "") ("attach" . "📎") ("TOC_3_gh" . "") ("noexport" . "❌") ("ignore" . "🚫") ("website" . "") ("java" . "") ("prolog" . "") ("scheme" . "λ") ("ocaml" . "") ("python" . "") ("kotlin" . "") ("elisp" . "") ("tex" . "") ("bib" . "") ("web" . "🔗") ("@stackexchange" . "") ("@stackoverflow" . "") ("@github" . "") ("@youtube" . "") ("@reddit" . "") ("@shopping" . "🛒") ("@audiophile" . "") ("@gift" . "") ("@inbox" . "") ("speaker" . "🔈"))
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
 '(org-variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Fira Sans")))))
