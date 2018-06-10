(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bibtex-BibTeX-entry-alist
   '(("Online" "Online Resource"
      (("title")
       ("url")
       ("urldate"))
      nil nil)
     ("Article" "Article in Journal"
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
       ("note")))))
 '(bibtex-autokey-name-year-separator "-")
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-titlewords-stretch 1)
 '(bibtex-autokey-year-title-separator "-")
 '(org-agenda-files
   '("/Users/jacobchaffin/Dropbox/org/todos/TODOs.org" "/Users/jacobchaffin/Dropbox/Documents/Courses/cs180/org/TODOs.org" "/Users/jacobchaffin/Dropbox/Documents/Courses/ling120c/org/TODOs.org" "/Users/jacobchaffin/Dropbox/Documents/Courses/hist109b/org/TODOs.org"))
 '(safe-local-variable-values
   '((mangle-whitespace . t)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (eval bibtex-set-dialect 'biblatex)
     (eval sh-set-shell "zsh")
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider--make-result-overlay . 1)
      (multiline-comment-handler . defun)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (org-ref-default-bibliography "~/Dropbox/Documents/Courses/ling120c/ling120c.bib"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

