# Configuration


## Preface


### About

[Literate Programming](https://en.wikipedia.org/wiki/literate_programming) is a method of writing computer programs where the composition, logic, and structure of the program are optimized for human comprehension. Introduced by Donald Knuth in his 1983 eponymous work, a literate program interoperates source code with macros, commentary, and documentation written in natural language prose. The source code is then extracted in a pre-compilation step known as *tangling*.

Where as a program is traditionally presented in a tree structure, a programmer implementing literate techniques arranges parts and forms the relations of these parts in the order of human logic.

A literate program is then structured like an essay or other work of literature, where ideas are connected in the form of a web rather than the unidirectional order of interpretation that is characteristic of a compiler.


### Influences

-   [Sacha Chua's Emacs Configuration](http://pages.sachachua.com/.emacs.d/sacha.html)
-   [Aaron Bedra's Emacs Configuration](http://aaronbedra.com/emacs.d/)
-   [wasamasa/dotemacs](https://github.com/Wasamasa/dotemacs/blob/master/init.org)
-   [Optimal Emacs Settings For Org Mode For Literal Programming](http://fgiasson.com/blog/index.php/2016/06/21/optimal-emacs-settings-for-org-mode-for-literate-programming/)


## Package Management


### straight.el

Currently we have a tumultuous relationship.


### Use Package

```emacs-lisp
(eval-when-compile
    (require 'use-package))
  (require 'diminish)
  (require 'bind-key)
```


### Default Packages

```emacs-lisp
(require 'cl)
(require 'dash)
```


## User Information

These values are initialized with the ’name’ and ’email’ environment variables, respectively<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>.

Emacs uses these variables to fill the mail header when sending emails in emacs, and various third-party packages rely on them for correct behavior.

```emacs-lisp
  (setq user-full-name "Jacob Chaffin"
        user-mail-address "jchaffin@ucla.edu")
```

Ensure that programs store emacs information in the cannonical directory.

```emacs-lisp
  (setq user-emacs-directory "~/.emacs.d/")
```


## Core


### Macros


#### `with-system`

This is a simple utility macro that evaluates code depending on operating system type.

Where as the top level `system-type` aliases are intended to simplify conditionals of the *if-then* form or *if-else-then* form, the `with-system` macro is intended for use in structures with a single conditional clause.

In emacs-lisp, this kind of statement can be expressed with the `when` macro<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>.

Credits to stack overflow user [gerstmann](https://stackoverflow.com/users/403018/gerstmann), who provided this solution in the following stack overflow [ticket](https://stackoverflow.com/a/26137517/6233622).

```emacs-lisp
(defmacro with-system (type &rest body)
  "If TYPE equals `system-type', evaluate BODY."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))
```


### Functions


#### Add multiple hooks

See Stack Overflow ticket [#7398216](https://stackoverflow.com/a/7400476/6233622)

```emacs-lisp
  (defun add-to-hooks (fun hooks)
          "Add function to hooks"
          (dolist (hook hooks)
           (add-hook hook fun)))
```


#### Join Strings with Separator

```emacs-lisp
(defun join (lst sep)
  (mapconcat 'identity lst sep))
```


## Environment


### macOS

I work on a macbook, so this block is where I'm loading all of my settings that rely on local filepaths, macOS applications, and external programs.

```emacs-lisp
(defconst *is-mac* (eq system-type 'darwin))
```


#### Computer Name

```emacs-lisp
(defun chaffin/computer-name-cmd ()
  (let* ((has-scutil (executable-find "scutil"))
	 (scutil-cmd (lambda () (shell-command-to-string "scutil --get ComputerName"))))
    (if has-scutil
	(replace-regexp-in-string "\n" "" (funcall scutil-cmd))
      nil)))

(defvar computer-name (chaffin/computer-name-cmd))
(defconst *is-hal* (string= computer-name "hal"))
```


#### Keybindings

macOS specific settings<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>.

Maps the modifier keys based on personal preferences. Also sets terminal coding system to "utf-8".

```emacs-lisp
(setq mac-command-modifier 'super
            mac-option-modifier 'meta
            ns-control-modifier 'control
            ns-function-modifier 'hyper)

(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
```


#### Reveal in OSX Finder

```emacs-lisp
  (use-package reveal-in-osx-finder
    :straight t
    :bind ("C-c z" . reveal-in-osx-finder))
```


#### OSX Dictionary

Provides an interface to *Dictionary.app* in Emacs.

```emacs-lisp
  (use-package osx-dictionary
    :straight t
    :bind (("C-c C-d" . osx-dictionary-search-word-at-point)
           ("C-c i" . osx-dictionary-search-input)))
```


#### Exec Path From Shell

Emacs relies heavily on shell environment variables.

These variables may not be picked up when launching emacs from a gui on a unix-like operating system.

The [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) package fixes this problem by copying user environment variables from the shell.

The `exec-path-from-shell` package only works with posix-compliant operating systems. This may or may not include Microsoft Windows<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup>.

However, the `exec-path-from-shell` instructions recommends loading the package on linux and macOS operating system. I don't use Windows all that often anyways, so that's fine with me.

The `:if` key of `use-package` offers us a really concise way for conditionally loading dependencies.

```emacs-lisp
  (use-package exec-path-from-shell
    ;; only load `exec-path-from-shell' package on macos and linux.
    :if (memq window-system '(mac ns))
    :straight t
    :config
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil))
```


### Other Operating Systems


#### Windows/PC

```emacs-lisp
(defconst *is-windows* (eq system-type 'windows))
```


#### Linux

```emacs-lisp
(defconst *is-linux* (eq system-type 'linux))
```


### Graphical Interace

There's some packages and commands I only want available when Emacs is running in a graphical interface. Things like image-rendering, large packages, and image rendering.

```emacs-lisp
  (defconst *is-gui* (display-graphic-p))
```


### Custom File

By default, Emacs customizations<sup><a id="fnr.5" class="footref" href="#fn.5">5</a></sup> done through the `customize` interface write to `user-init-file`.

While I usually prefer configuring emacs programmatically, settings that depend on resources outside of this repository, such as org-agenda files, will impact portability and potentially break on other machines.

```emacs-lisp
(setq custom-file "~/.emacs.d/custom/custom.el")
(load custom-file)
```


### Backup Files

This might come back to bite me one day but I never use them.

```emacs-lisp
(setq make-backup-files nil)
```


### Use Emacs Terminfo

Setting this variable to false forces Emacs to use internal terminfo, rather than the system terminfo.

```emacs-lisp
  (setq system-uses-terminfo nil)
```


## Org Mode


### Org Prettify

These settings subjectively improve the overall viewable-ness of org-mode buffers.

```emacs-lisp
  (add-hook #'org-mode-hook (lambda ()
                              (auto-fill-mode)
                              (visual-line-mode)))
(setq org-image-actual-width 400)
  (use-package org-bullets
    :straight t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package toc-org
  :straight t
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))
```


#### Improve Legibility

```emacs-lisp
  (add-hook #'org-mode-hook (lambda ()
                              (auto-fill-mode)
                              (visual-line-mode)))
```


#### Image Size

Make images compact.

```emacs-lisp
(setq org-image-actual-width 400)
```


#### Org Bullets

Use UTF-8 Bullets for Org-mode headings.

```emacs-lisp
  (use-package org-bullets
    :straight t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

```


<a id="org-beautify-theme"></a>

#### Org Beautify Theme

```emacs-lisp
  (use-package org-beautify-theme
    :ensure nil
    :defines org-beautify-theme-use-box-hack
    :config
    (setq org-beautify-theme-use-box-hack nil)
    (add-to-list 'custom-enabled-themes 'org-beautify)
    (load-theme 'org-beautify t))
```


#### TOC Org

```emacs-lisp
(use-package toc-org
  :straight t
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))
```


### Org Utilities

```emacs-lisp
  (use-package org-agenda
    :recipe org
    :defer-install t
    :bind (("C-c a" . org-agenda)))
  (use-package org-pomodoro
    :straight t
    :bind (:map org-mode-map
                ("C-c M-RET p" . org-pomodoro))
    :config
    (setq org-pomodoro-audio-player "afplay"
          org-pomodoro-tick-sound
          (expand-file-name "~/.emacs.d/resources/clock-ticking-2.wav")
          ;; Start Settings
          org-pomodoro-start-sound-p t ;; enable starting sound
          org-pomodoro-start-sound-args "--volume 0.08"
          org-pomodoro-start-sound
          (expand-file-name "~/.emacs.d/resources/Victory.wav")
          ;; Finished Settings
          org-pomodoro-finished-sound-args "--volume 0.2"
          org-pomodoro-finished-sound
          (expand-file-name "~/.emacs.d/resources/Waves.wav")
          ;; Short Break Settings
          org-pomodoro-short-break-length 5
          org-pomodoro-short-break-sound-args "--volume 0.2"
          org-pomodoro-short-break-sound org-pomodoro-finished-sound
          ;; Long Break Settings
          org-pomodoro-long-break-length 15
          org-pomodoro-long-break-sound-args "--volume 0.2"
          org-pomodoro-long-break-sound
          (expand-file-name "~/.emacs.d/resources/Complete.wav")))
  (use-package org-ref
    :if *is-mac*
    :straight t
    :config
    (progn
      (setq org-ref-bibliography-notes "~/Dropbox/org/papers/notes.org"
            org-ref-default-bibliography '("~/Dropbox/org/papers/references.bib")
            org-ref-pdf-directory "~/Dropbox/org/papers/pdfs/")
      (require 'org-ref-pdf)
      (require 'org-ref-latex)
      (require 'org-ref-reftex)
      (require 'org-ref-utils)
      (require 'org-ref-url-utils)
      (require 'org-ref-pubmed)
      (require 'org-ref-scopus)
      (require 'org-ref-scifinder)
      (require 'org-ref-citeproc)
      (require 'org-ref-sci-id)
      (require 'org-ref-isbn)
      (require 'org-ref-wos)
      (require 'org-ref-worldcat)
      (require 'x2bib)))
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(require 'org-ref-ivy)
(require 'org-ref-ivy-cite)
(defun chaffin/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'chaffin/org-ref-open-pdf-at-point)

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(defun chaffin/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))
```


#### Org Agenda

```emacs-lisp
  (use-package org-agenda
    :recipe org
    :defer-install t
    :bind (("C-c a" . org-agenda)))
```


#### Org Pomodoro

```emacs-lisp
  (use-package org-pomodoro
    :straight t
    :bind (:map org-mode-map
                ("C-c M-RET p" . org-pomodoro))
    :config
    (setq org-pomodoro-audio-player "afplay"
          org-pomodoro-tick-sound
          (expand-file-name "~/.emacs.d/resources/clock-ticking-2.wav")
          ;; Start Settings
          org-pomodoro-start-sound-p t ;; enable starting sound
          org-pomodoro-start-sound-args "--volume 0.08"
          org-pomodoro-start-sound
          (expand-file-name "~/.emacs.d/resources/Victory.wav")
          ;; Finished Settings
          org-pomodoro-finished-sound-args "--volume 0.2"
          org-pomodoro-finished-sound
          (expand-file-name "~/.emacs.d/resources/Waves.wav")
          ;; Short Break Settings
          org-pomodoro-short-break-length 5
          org-pomodoro-short-break-sound-args "--volume 0.2"
          org-pomodoro-short-break-sound org-pomodoro-finished-sound
          ;; Long Break Settings
          org-pomodoro-long-break-length 15
          org-pomodoro-long-break-sound-args "--volume 0.2"
          org-pomodoro-long-break-sound
          (expand-file-name "~/.emacs.d/resources/Complete.wav")))
```


#### Org Ref

```emacs-lisp
  (use-package org-ref
    :if *is-mac*
    :straight t
    :config
    (progn
      (setq org-ref-bibliography-notes "~/Dropbox/org/papers/notes.org"
            org-ref-default-bibliography '("~/Dropbox/org/papers/references.bib")
            org-ref-pdf-directory "~/Dropbox/org/papers/pdfs/")
      (require 'org-ref-pdf)
      (require 'org-ref-latex)
      (require 'org-ref-reftex)
      (require 'org-ref-utils)
      (require 'org-ref-url-utils)
      (require 'org-ref-pubmed)
      (require 'org-ref-scopus)
      (require 'org-ref-scifinder)
      (require 'org-ref-citeproc)
      (require 'org-ref-sci-id)
      (require 'org-ref-isbn)
      (require 'org-ref-wos)
      (require 'org-ref-worldcat)
      (require 'x2bib)))
```


##### Org Ref Ivy

```emacs-lisp
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(require 'org-ref-ivy)
(require 'org-ref-ivy-cite)
```


##### Open Pdf at point

See [Using Doc View or PDF Tools](https://github.com/jkitchin/org-ref/blob/master/org-ref.org#using-doc-view-or-pdf-tools) section of `org-ref` documentation.

```emacs-lisp
(defun chaffin/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'chaffin/org-ref-open-pdf-at-point)

```


#### Org ID


##### Custom ID

```emacs-lisp
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
```

-   Get Custom ID

    From [Emacs Org Mode Generate Ids](https://writequit.org/articles/emacs-org-mode-generate-ids.html) blogpost:
    
    ```emacs-lisp
    (defun chaffin/org-custom-id-get (&optional pom create prefix)
      "Get the CUSTOM_ID property of the entry at point-or-marker POM.
       If POM is nil, refer to the entry at point. If the entry does
       not have an CUSTOM_ID, the function returns nil. However, when
       CREATE is non nil, create a CUSTOM_ID if none is present
       already. PREFIX will be passed through to `org-id-new'. In any
       case, the CUSTOM_ID of the entry is returned."
      (interactive)
      (org-with-point-at pom
        (let ((id (org-entry-get nil "CUSTOM_ID")))
          (cond
           ((and id (stringp id) (string-match "\\S-" id))
            id)
           (create
            (setq id (org-id-new (concat prefix "h")))
            (org-entry-put pom "CUSTOM_ID" id)
            (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
            id)))))
    ```


### Org PDF


#### Pdf Tools

```emacs-lisp
(use-package pdf-tools
  :straight t
  :defer t
  :config
  (pdf-tools-install))
```


#### Org PDF View

```emacs-lisp
  (use-package org-pdfview
    :after pdf-tools
    :straight t
    :mode (("\\.pdf\\'" . pdf-view-mode)))
```

Now we can open pdf files with `org-pdfview`<sup><a id="fnr.6" class="footref" href="#fn.6">6</a></sup>:

```emacs-lisp
(with-eval-after-load 'org-pdfview
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))
```


### Org Export


#### LaTeX Backend


##### Latexmk

[Latexmk](https://ctan.org/pkg/latexmk?lang=en) automates the proces of building LaTeX documents to pdf.

It can be done through the `org-mode` latex export dispatcher in a single command:

```emacs-lisp
(setq org-latex-pdf-process
    '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -synctex=1 -pdf -bibtex -f %f"))
```


##### Prefer user provided labeling system

```emacs-lisp
(setq org-latex-prefer-user-labels t)
```


##### Org LaTeX Default Packages

```emacs-lisp
<<org-export-latex-default-packages-natbib>>
```

-   Natbib

    Add [natbib](https://ctan.org/pkg/natbib?lang=en) package, as it's the easiest way to get BibTeX support through [org-ref](https://github.com/jkitchin/org-ref/blob/master/org-ref.org) and other tools for LaTeX in Emacs.
    
    ```emacs-lisp
    (add-to-list 'org-latex-default-packages-alist '("" "natbib" "") t)
    ```


##### Custom Classes

-   Org Per File Class

    Adds a class for exporting to pdf using latex backend without importing the default `ox-latex` packages. This allows the export settings of a particular file to be completely insulated from most external configuration.
    
    ```emacs-lisp
    (add-to-list 'org-latex-classes
    	     '("per-file-class"
    	       "\\documentclass{scrartcl}
    	       [NO-DEFAULT-PACKAGES]
    	       [EXTRA]"
    	       ("\\section{%s}" . "\\section*{%s}")
    	       ("\\subsection{%s}" . "\\subsection*{%s}")
    	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
    	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ```


#### GitHub Flavored Markdown

```emacs-lisp
  (use-package ox-gfm
    :straight t
    :init
    (with-eval-after-load 'org-mode
      (require 'ox-gfm)))
```


#### Org YouTube

From [Endless Parentheses](http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html) blogpost:

Embed YouTube Links in iframe.

```emacs-lisp
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))
```


#### Ox Extra

Ignores headlines tagged "ignore". Unlike "noexport", the contents and subtrees of the ignored headlines will be retained.

```emacs-lisp
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
```


## (Better) Defaults


### Better Utilities


#### Dired+

Adds extensions and functionality to dired mode.

```emacs-lisp
  (use-package dired+
    :straight t)
```


#### Undo Tree

```emacs-lisp
  (use-package undo-tree
    :straight t
    :init
    (global-undo-tree-mode))
```


#### Projectile

```emacs-lisp
  (use-package projectile
    :straight t
    :config
    (projectile-mode))
```


#### Restart Emacs

The [restart-emacs](https://github.com/iqbalansari/restart-emacs) package allows quickly rebooting Emacs from within Emacs.

```emacs-lisp
  (use-package restart-emacs
    :straight t
    :bind (("C-x C-r" . restart-emacs)))
```


#### Popwin

```emacs-lisp
(use-package popwin
  :straight t
  :config (popwin-mode 1))
```


### Better Display


#### Page Break Lines

Global minor-mode that turns `^L` form feed characters into horizontal line rules.

```emacs-lisp
  (use-package page-break-lines
    :straight t
    :init
    (global-page-break-lines-mode))
```


#### Splash Screen Replacement.

The default splash screen is great when you're starting out, but it's more so an annoyance than anything else once you know you're around.


##### Enable Emojis for org tags in dashboard agenda widget

```emacs-lisp
    (use-package emojify
      :straight t
      :init (global-emojify-mode))
```


##### Emacs Dashboard

[Dashboard](https://github.com/rakanalh/emacs-dashboard) is a highly customizable splash screen replacement library used in the popular [spacemacs](https://github.com/syl20bnr/spacemacs) framework. It's a nice way of consolidating any combination of tasks, agenda items, bookmarks, and pretty much any other enumerable list that one may use in the wacky world of Emacs.

```emacs-lisp
  (use-package dashboard
    :straight t
    :init
    (with-eval-after-load 'page-break-lines
      (if (not (global-page-break-lines-mode))
          (global-page-break-lines-mode)))
    :config
    (dashboard-setup-startup-hook))

  (progn
    (add-to-list 'dashboard-items '(agenda) t)
    (setq dashboard-banner-logo-title "Welcome Back, MasterChaff"
          dashboard-items '(( agenda . 10)
                            ( projects . 5)
                            ( recents . 5)
                            ( bookmarks . 5))))
```


#### Inhibit Scratch Buffer

```emacs-lisp
  (setq initial-scratch-message nil
        inhibit-startup-message t
        inhibit-startup-screen t)
```


#### Menu Bar, Tool Bar, Scroll Bar

Disable scroll bars and tool bar on all system types.

On macOS, the menu bar is integrated into the UI.

Disabling it will just empty the menu tab options for Emacs.app, so we'll leave it there.

```emacs-lisp
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (unless (eq system-type 'darwin)
  (menu-bar-mode -1))
```


#### Truncate Lines

Not sure this is doing anything&#x2026;

```emacs-lisp
  (setq-default truncate-lines nil)
```


### Better Garbage Collection

Consider the following from the documentation:

> By binding this temporarily to a large number, you can effectively prevent garbage collection during a part of the program.

When I first read how the default garbage collection interval in Emacs is notoriously low, I added an arbitrary number of zeros to the default value and called it a day. However, because I'm writing this monolithic configuration and making a lot of mistakes in the process, I've had to start Emacs with essentially its default settings pretty frequently and I've noticed the lag time I occasionally experience when searching long documents is essentially nonexistent in vanilla Emacs. After reading this [blogpost](http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/), it seems that jacking up the GC interval may actually be the *cause* of the lagtime rather than contributing to the solution.

```emacs-lisp
(defun chaffin/rev-up-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun chaffin/rev-down-gc ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'chaffin/rev-up-gc)
(add-hook 'minibuffer-exit-hook 'chaffin/rev-down-gc)
```


### Better Encryption


#### GnuTLS

-   See [wasamasa/dotfiles](https://github.com/wasamasa/dotemacs/blob/master/init.org#gnutls)

```emacs-lisp
  (setq gnutls-min-prime-bits 4096)
```


#### Use GPG2

Set GPG program to 'gpg2'.

```emacs-lisp
  (when *is-mac*
    (setq epg-gpg-program "gpg2"))
```


#### Disable External Pin Entry

Switching between Emacs and an external tools is annoying.

By default, decrypting gpg files in Emacs will result in the pin entry window being launched from the terminal session.

By disabling the agent info, we can force Emacs to handle this internally<sup><a id="fnr.7" class="footref" href="#fn.7">7</a></sup>.

```emacs-lisp
  (setenv "GPG_AGENT_INFO" nil)
```

Or so I thought&#x2026;


#### Internal Pinentry Problem and Solution

While I couldn't figure out how to get Emacs to handle gpg pinentry internally, I was able to still find a satisfactory solution using the `pinentry-mac` tool.

Note that this solution requires macOS and using gpg2 for encryption.

See ticket [#1437](https://github.com/Homebrew/homebrew-core/issues/14737) from the [Homebrew/homebrew-core](https://github.com/Homebrew/homebrew-core) repository.

    brew install pinentry-mac
    echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf
    killall gpg-agent


### Better Commands


##### Alias Yes Or No

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
```


## Editing


### Indentation

Tabs are the bane of humanity<sup><a id="fnr.8" class="footref" href="#fn.8">8</a></sup>. [Don't @ me](http://www.urbandictionary.com/define.php?term=dont%20%40%20me).

```emacs-lisp
  (setq tab-width 2
        indent-tabs-mode nil)
```


#### `highlight-indent-guides`

[Highlight Indent Guides](https://github.com/darthfennec/highlight-indent-guides) sublime-like indentation guides.

*Commented out because of bug that leaves a trail of solid white line marks on the indent guide overlay.*

```emacs-lisp
 (use-package highlight-indent-guides
    :straight t
    :init
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character))
```


### YASnippet

YASnippet is a template system based off the TextMate snippet syntax.

Let's begin by creating a variable for our personal snippets directory.

```emacs-lisp
  (setq user-snippets-dir (concat user-emacs-directory "snippets"))
```

After installation and enabling the package, add the personal snippets directory to the list of directories where YASnippet should look for snippets.

```emacs-lisp
  (use-package yasnippet
    :straight t
    :init
    (yas-global-mode 1)
    :config
    (push 'user-snippets-dir yas-snippet-dirs))
```

YASnippet can also be used as a non-global minor mode on a per-buffer basis.

Invoking `yas-reload-all` will load the snippet tables, and then calling `yas-minor-mode` from the major mode hooks will load the snippets corresponding to the major mode of the current buffer mode.

    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)


### Flycheck

[On the fly](https://github.com/Flycheck/Flycheck) syntax highlighting.

```emacs-lisp
  (use-package flycheck
    :defer-install t
    :init
    (setq flycheck-global-modes nil)
    :config
      (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                    flycheck-emacs-lisp-load-path 'inherit)
             (use-package flycheck-color-mode-line
      	 :straight t
      	 :init
      	 (add-hook 'flycheck-mode 'flycheck-color-mode-line-mode)))
```


#### Flycheck Color Mode Line

Colors the modeline according to current Flycheck state of buffer.

```emacs-lisp
       (use-package flycheck-color-mode-line
	 :straight t
	 :init
	 (add-hook 'flycheck-mode 'flycheck-color-mode-line-mode))
```


#### Flycheck Package

[Flycheck Package](https://github.com/purcell/flycheck-package) requires `package.el` to be enabled, so it's incompatible with `straight.el`.

```emacs-lisp
  (use-package flycheck-package
    :straight t
    :init
    (eval-after-load 'flycheck
      '(flycheck-package-setup)))
```


#### Flycheck in Org Special Edit Buffers

```emacs-lisp
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name))) ;; (1)
      ad-do-it                            ;; (2)
      ;; (3)
      (setq buffer-file-name file-name)))
```


### Company

Emacs has two popular packages for code completion &#x2013; [autocomplete](https://github.com/auto-complete/auto-complete) and [company](https://github.com/company-mode/company-mode). This reddit [thread](https://www.reddit.com/r/emacs/comments/2ekw22/autocompletemode_vs_companymode_which_is_better/) was enough for me to go with company.

If you need more convincing, [company-mode/company-mode#68](https://github.com/company-mode/company-mode/issues/68) offers a comprehensive discussion on the two.

The ticket is from the ‘company-mode‘ repository, so there's probably some bias there, but company-mode hasn't provided any reason for me reconsider my choice.

```emacs-lisp
(global-company-mode)
(setq company-tooltip-limit 20
      company-tooltip-align-annotations t
      company-idle-delay .3
      company-begin-commands '(self-insert-command))
```


#### Company Statistics

[Company statistics](https://github.com/company-mode/company-statistics) uses a persisent store of completions to rank the top candidates for completion.

```emacs-lisp
  (use-package company-statistics
    :straight t
    :config
    ;; Alternatively,
    ;; (company-statistics-mode)
    (add-hook 'after-init-hook 'company-statistics-mode))
```


#### Company Quick Help

[Company Quick Help](https://github.com/expez/company-quickhelp) emulates `autocomplete` documentation-on-idle behavior, but using the less-buggy `pos-tip` package rather than `popup-el`.

```emacs-lisp
  (use-package company-quickhelp
    :defer t
    :commands (company-quickhelp-manual-begin)
    :bind
    (:map company-active-map
          ("C-c h" . company-quickhelp-manual-begin))
    :config
    (company-quickhelp-mode 1))
```


#### Company Dict

```emacs-lisp
  (use-package company-dict
   :straight t
   :init
   (add-to-list 'company-backends 'company-dict)
   :config
   (setq company-dict-enable-fuzzy t
         company-dict-enable-yasnippet t))
```


## Utilities


### Image+

[Image+](https://github.com/mhayashi1120/Emacs-imagex) provides extensions for image file manipulation in Emacs.

```emacs-lisp
  (use-package image+
    :straight t
    :if *is-gui*
    :after image
    :config
    (eval-after-load 'image+
      `(when (require 'hydra nil t)
         (defhydra imagex-sticky-binding (global-map "C-x C-l")
           "Manipulating image"
           ("+" imagex-sticky-zoom-in "zoom in")
           ("-" imagex-sticky-zoom-out "zoom out")
           ("M" imagex-sticky-maximize "maximize")
           ("O" imagex-sticky-restore-original "resoure orginal")
           ("S" imagex-sticky-save-image "save file")
           ("r" imagex-sticky-rotate-right "rotate right")
           ("l" imagex-sticky-rotate-left "rotate left")))))
```


### Ivy


#### Ivy Config

[Ivy](https://github.com/abo-abo/swiper) is a completion and selection framework in the same vein as helm. It doesn't have the same kind of ecosystem or interopability, but its easy to configure, offers a minimalistic interface, and is every bit as good of a completion tool as helm is, if not better.

I prefer the default regex matcher, but if you want fuzzy matching as a fallback or replacement checkout this article on [better fuzzmatching support with ivy](https://oremacs.com/2016/01/06/ivy-flx/).

```emacs-lisp
  (use-package ivy
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-initial-inputs-alist nil
          ivy-re-builders-alist
	  '((t . ivy--regex-plus)))
      (use-package ivy-hydra
        :ensure hydra)
    (use-package historian
      :straight t)
    (use-package ivy-historian
      :after historian 
      :straight t))
```


#### Ivy Hydra

```emacs-lisp
  (use-package ivy-hydra
    :ensure hydra)
```


#### Ivy Historian

```emacs-lisp
(use-package ivy-historian
  :after historian 
  :straight t)
```


#### Counsel

```emacs-lisp
       (use-package counsel
         :straight t
         :bind
         (("C-c C-r" . ivy-resume)
          ("C-`" . ivy-avy)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
         :init
           (progn
               (use-package ivy
                 :config
                 (ivy-mode 1)
                 (setq ivy-use-virtual-buffers t
                       ivy-initial-inputs-alist nil
                       ivy-re-builders-alist
             	  '((t . ivy--regex-plus)))
                   (use-package ivy-hydra
                     :ensure hydra)
                 (use-package historian
                   :straight t)
                 (use-package ivy-historian
                   :after historian 
                   :straight t))
               (use-package swiper
               :bind
               (("\C-s" . swiper))
               :init
                 (use-package avy
                   :straight t))
               (use-package counsel-projectile
                 :straight t
                 :init
                 (progn
                   (counsel-projectile-on)))
                (use-package counsel-osx-app
                  :if *is-mac*
                  :straight t
                  :bind (("C-c o a" . counsel-osx-app)))
               (use-package counsel-dash
                 :if *is-mac*
                 :straight t
                 :init (defun counsel-dash-at-point ()
                         "Counsel dash with selected point."
                         (interactive)
                         (counsel-dash
                          (if (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end))
                            (substring-no-properties (or (thing-at-point 'symbol) "")))))
                 :config
                 (setq counsel-dash-docsets-path
                       (expand-file-name "~/Library/Application\sSupport/Dash/DocSets")))
               (use-package smex
                 :straight t
                 :init (setq-default smex-history-length 32))))
```


#### Swiper

```emacs-lisp
  (use-package swiper
  :bind
  (("\C-s" . swiper))
  :init
    (use-package avy
      :straight t))
```


#### Swiper Avy

```emacs-lisp
  (use-package avy
    :straight t)
```


#### Counsel-Projectile

[Counsel Projectile](https://github.com/ericdanan/counsel-projectile) provides a project management interface via ivy and friends.

```emacs-lisp
  (use-package counsel-projectile
    :straight t
    :init
    (progn
      (counsel-projectile-on)))
```


#### Smex

```emacs-lisp
  (use-package smex
    :straight t
    :init (setq-default smex-history-length 32))
```


#### Counsel OSX App

```emacs-lisp
   (use-package counsel-osx-app
     :if *is-mac*
     :straight t
     :bind (("C-c o a" . counsel-osx-app)))
```


#### Counsel Dash

```emacs-lisp
  (use-package counsel-dash
    :if *is-mac*
    :straight t
    :init (defun counsel-dash-at-point ()
            "Counsel dash with selected point."
            (interactive)
            (counsel-dash
             (if (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
               (substring-no-properties (or (thing-at-point 'symbol) "")))))
    :config
    (setq counsel-dash-docsets-path
          (expand-file-name "~/Library/Application\sSupport/Dash/DocSets")))
```


### Deft

[Deft](https://jblevins.org/projects/deft/) is a notetaking application for Emacs.

```emacs-lisp
  (use-package deft
    :straight t
    :bind ("C-x C-n" . deft)
    :config
    (setq deft-extensions '("org")
          deft-directory "~/Dropbox/org/notes"
          deft-use-filename-as-title t
          deft-default-extension "org"))
```


### Wakatime

```emacs-lisp
  (use-package wakatime-mode
    :if (and *is-mac* (or (string= (downcase computer-name) "hal") (string= (downcase computer-name) "junior")))
    :straight t
    :init
    (add-hook 'prog-mode-hook 'wakatime-mode)
    :config
    (progn
      (setq wakatime-cli-path
            (expand-file-name "~/.local/lib/python3.6/site-packages/wakatime/cli.py")
            wakatime-python-bin
            (expand-file-name "~/.pyenv/shims/python"))

      (defun wakatime-dashboard ()
        (interactive)
        (browse-url "https://wakatime.com/dashboard"))))
```


## User Interface


### Cursor


#### Vertical Bar

Set the cursor to a bar. The default is too thin for my liking. Set the width to 4px. Also remove the cursor in inactive windows.

```emacs-lisp
       (setq-default cursor-type '(bar . 4)
                     cursor-in-non-selected-windows 'nil
                     x-stretch-cursor t
                     line-spacing 2)
```


#### Disable Blink

Ultimately, I'd like to set a longer blink interval, like the "phase" `caret_style` setting in Sublime Text.

```emacs-lisp
       (blink-cursor-mode -1)
```


#### Smart Cursor Color

```emacs-lisp
       (use-package smart-cursor-color
         :straight t
         :config
         (smart-cursor-color-mode +1))
```


### Theme


#### Enable Custom Themes

This disables Emacs asking questions about loading a new theme.

```emacs-lisp
 (setq custom-safe-themes t)
```


#### Zenburn Theme

```emacs-lisp
  (use-package zenburn-theme
    :straight t
    :config (load-theme 'zenburn))
```


#### Load Themes

Zenburn theme has to be loaded before [Org Beautify Theme](#org-beautify-theme) to preserve compatibility.


### Modeline


#### Display Time

Show the time in the modeline.

```emacs-lisp
  (display-time-mode 1)
```


#### Smart-Mode-Line

```emacs-lisp
  (use-package smart-mode-line
    :straight t
    :init
    (smart-mode-line-enable)
    :config
    (setq sml/mode-width 0
          sml/name-width 20
          sml/not-confirm-load-theme t)
    (setf rm-blacklist "")
    (sml/setup))
```


#### Mode Icons

```emacs-lisp
  (use-package mode-icons
    :straight t
    :if *is-gui*
    :after smart-mode-line
    :config
    (mode-icons-mode))
```


### All The Icons

[All The Icons](https://github.com/domtronn/all-the-icons.el) is a utility package for icons in Emacs.

```emacs-lisp
  (use-package all-the-icons
    :if *is-gui*
    :straight t
    :init
    (unless (straight-check-package "all-the-icons")
      (all-the-icons-install-fonts)))

  (use-package all-the-icons-ivy
      :after all-the-icons ivy
      :straight t
      :if *is-gui*
      :init
      (all-the-icons-ivy-setup))

  (use-package all-the-icons-dired
    :if *is-gui*
    :straight t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
```


### Terminal


#### Multi-Term

```emacs-lisp
(use-package multi-term
       :straight t
       :bind
       (("C-c M-RET t" . multi-term)
	("C-c M-RET p" . multi-term-prev)
	("C-c M-RET n" . multi-term-next)
	("C-c M-RET o" . multi-term-dedicated-toggle))
       :config
       (progn
	 (setq multi-term-program
	     (if (string= shell-file-name "/bin/sh")
		 "/bin/bash"
	       shell-file-name))))
```


### Frame Font

Use the default monospaced font for each operating system.

```emacs-lisp
 (cond (*is-linux*
  (set-frame-font "Ubuntu Mono 12" nil t))
 (*is-windows*
  (set-frame-font "Lucida Sans Typewriter 12" nil t))
 ((eq system-type 'darwin)
  (set-frame-font "SF Mono 12" nil t))
 (t
  (set-frame-font "Menlo 12" nil t)))
```


### Sunshine

```emacs-lisp
  (use-package sunshine
    :straight t
    :config
    (setq sunshine-location "90024,USA"))
```


### Theme Changer

```emacs-lisp
  (use-package theme-changer
    :straight t
    :config
    (change-theme 'zenburn-theme 'anti-zenburn-theme))
```


### Emojify

```emacs-lisp
  (use-package emojify
    :straight t
    :init (global-emojify-mode))
```


## Version Control


### Magit

[Magit](https://github.com/magit/magit) describes itself as one of two git porcelains, the other being git itself.

A git porcelain is jargon for a program that features a user-friendly vcs interface, as opposed to lower-level scripting commands.

It's not a vitrified ceramic commonly used for decorative tableware. Magit would not be very good at that.

As a git client though, magit is awesome.

```emacs-lisp
  (use-package magit
    :straight t
    :bind (("C-c v b" . magit-blame)
           ("C-c v C" . magit-clone)
           ("C-c v c" . magit-checkout)
           ("C-c v i" . magit-init)
           ("C-c v m" . magit-merge)
           ("C-c v l" . magit-log-buffer-file)
           ("C-c v p" . magit-pull)
           ("C-c v P" . magit-push)
           ("C-c v v" . magit-status))
    :config (setq magit-save-repository-buffers 'dontask))
```


#### Magithub

[Magithub](https://github.com/vermiculus/github) offers an interface to github to complement magit.

```emacs-lisp
  (use-package magithub
    :after magit
    :straight t
    :commands magithub-dispatch-popup
    :bind (:map magit-status-mode-map
                ("@" . magithub-dispatch-popup))
    :config
    (progn
      (magithub-feature-autoinject t)))
```


### gist.el

Emacs integration for gist.github.com.

[Gist](https://github.com/defunkt/gist.el) requires generating a personal access token with `gist` scope, and optionally `user` and `repo` scopes.

```emacs-lisp
  (use-package gist
    :straight t
    :bind (("C-c C-g l" . gist-list)
           ("C-c C-g r" . gist-region)
           ("C-c C-g b" . gist-buffer)
           ("C-c C-g p" . gist-buffer-private)
           ("C-c C-g B" . gist-region-or-buffer)
           ("C-c C-g P" . gist-region-or-buffer-private)))
```


### git-timemachine

Travel back in time (to your last commit).

```emacs-lisp
  (use-package git-timemachine
    :straight t
    :bind
    ("C-x v t" . git-timemachine-toggle)
    :config
    (setq git-timemachine-abbreviation-length 7))
```


### git-messenger

Pop-up feature for viewing the last git commit.

```emacs-lisp
  (use-package git-messenger
    :straight t
    :bind
    (("C-c v m" . git-messenger:popup-message)))
```


### git modes

```emacs-lisp
  (use-package git-modes
    :straight t)
```


## Web Browsing


### osx-browse

This library provides several useful commands for using the Google Chrome, Safari, and Firefox web browsers on macOS.

```emacs-lisp
  (use-package osx-browse
    :straight t
    :if *is-mac*
    :config
    (osx-browse-mode 1))
```


## Email


### Mu

```emacs-lisp
  (use-package mu4e
    :if *is-mac*
    :load-path "/usr/local/Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu/mu4e"
    :config
    (progn
      (setq mu4e-maildir (expand-file-name "~/.mail")
            mu4e-context-policy 'pick-first
            mu4e-compose-policy nil
            mu4e-get-mail-command "offlineimap -q -u quiet"
            mu4e-show-images t
            mu4e-show-addresses t)

       ;; smtp settings
       (setq mu4e-send-mail-function 'smtp-mail-send-it
       mu4e-default-smtp-server "smtp.gmail.com"
       smtpmail-smtp-service 587
       smtp-mail-smtp-stream-type 'ssl)


       (setq mu4e-contexts
             `( ,(make-mu4e-context
                  :name "private"
                  :match-func (lambda (msg)
                                (when msg
                                  mu4e-message-contact-field-matches msg
                                  :to "jchaffin57@gmail.com"))
                  :leave-func (lambda ()
                                (mu4e-message "Leaving Gmail Account"))
                  :vars '( (mu4e-reply-to-address "jchaffin@ucla.edu")
                           ( user-mail-address . "jchaffin57@gmail.com" )
                           ( user-full-name . "Jacob Chaffin" )
                           ( mu4e-drafts-folder . "/private/[Gmail].Drafts" )
                           ( mu4e-sent-folder . "/private/[Gmail].Trash" )
                           ( mu4e-refile-folder . "/archived")
                           ( mu4e-compose-signature .
                                                    (concat
                                                     "Jacob Chaffin\n"
                                                     "UCLA 19\n"
                                                     "Linguistics and Computer Science\n"
                                                     "jchaffin@ucla.edu\n"))))

                ,(make-mu4e-context
                  :name "school"
                  :enter-func (lambda () (mu4e-message "Switching to UCLA Mail"))
                  :leave-func (lambda () (mu4e-message "Leaving UCLA Mail"))
                  :match-func  (lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches msg
                                                                       :to (or "jchaffin@ucla.edu" "jchaffin@g.ucla.edu"))))
                  :vars '( (user-mail-address . "jchaffin@ucla.edu" )
                           (user-full-name . "Jacob Chaffin" )
                           (mu4e-compose-signature . (concat
                                                      "Jacob Chaffin\n"
                                                      "UCLA 19\n"
                                                      "Linguistics and Computer Science\n"
                                                      "jchaffin@ucla.edu\n"
                                                      "(650)-380-3288\n"))))))))

```


## Programming Languages


### Lisp


#### Parens

-   [Paredit and Smartparens](https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens)


##### Paredit

We could use `:init` key to hook the `enable-paredit-mode` function to each of the implementing languages like is done [here](https://github.com/tomjakubowski/.emacs.d/blob/master/init.el), but I think adding the hook in the configuration block of the programming language is easier to follow and offers more meaningful semantics.

```emacs-lisp
  (use-package paredit
    :straight t
    :diminish paredit-mode
    :config
    (use-package eldoc
      :straight t
      :config
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round))
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t))
```


##### Smartparens

Like paredit, [smartparens](https://github.com/Fuco1/smartparens) is a minor-mode for managing parens pairs. However, it also offers support for curly brackets in JavaScript objects, angle brackets in HTML, and most other major programming languages. I think I the "delete-on-command" behavior of paredit for lisp programming, but in languages where locating unmatched pairs is less comparable to searching for a needle in a haystack, smartparens are a great productivity tool.

```emacs-lisp
  (use-package smartparens
    :straight t
    :init
    (require 'smartparens-config))
```


#### Hlsexp

Minor mode to highlight s-expresion.

```emacs-lisp
  (use-package hl-sexp
    :straight t)
```


### Common-Lisp

Configuration for emacs-lisp.

```emacs-lisp
  (use-package lisp-mode
    :after paredit
    :config
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'hl-sexp-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode))
```


#### Slime

[SLIME](https://common-lisp.net/project/slime/) is The Superior Lisp Interaction Mode for Emacs.

```emacs-lisp
  (use-package slime
    :commands slime
    :defines (slime-complete-symbol*-fancy slime-completion-at-point-functions)
    :straight t
    :if *is-mac*
    :init
    (progn
      (setq slime-contribs '(slime-asdf
                             slime-fancy
                             slime-indentation
                             slime-sbcl-exts
                             slime-scratch)
            inferior-lisp-program "sbcl"
            ;; enable fuzzy matching in code buffer and SLIME REPL
            slime-complete-symbol*-fancy t
            slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
      (defun slime-disable-smartparens ()
        (smartparents-strict-mode -1)
        (turn-off-smartparens-mode))
      (add-hook 'slime-repl-mode-hook #'slime-disable-smartparens)))
```


### Clojure


#### Clojure Mode ([repository](https://gihub.com/clojure-emacs/clojure-mode))

Provides key bindings and code colorization for Clojure(Script).

```emacs-lisp
  (use-package clojure-mode
    :straight t
    :mode (("\\.edn$" . clojure-mode)
           ("\\.cljs$" . clojurescript-mode)
           ("\\.cljx$" . clojurex-mode)
           ("\\.cljsc$" . clojurec-mode))
    :config
    (add-hook 'clojure-mode-hook #'enable-paredit-mode)
      (use-package cljsbuild-mode
        :straight t
        :init
        (add-to-hooks #'cljsbuild-mode '(clojure-mode clojurescript-mode)))
      (use-package elein
        :straight t))
```


#### ClojureScript


##### Lein Cljsbuild

Minor mode offering `lein cljsbuild` commands for the Leiningen [plugin](https://github.com/emezeske/lein-cljsbuild).

```emacs-lisp
  (use-package cljsbuild-mode
    :straight t
    :init
    (add-to-hooks #'cljsbuild-mode '(clojure-mode clojurescript-mode)))
```


##### elein

[Elein](https://github.com/remvee/elein) rovides support for leiningen commands in Emacs.

```emacs-lisp
  (use-package elein
    :straight t)
```


#### Clojure Mode Extra Font Locking

Additional syntax highlighting for `clojure-mode`.

```emacs-lisp
  (use-package clojure-mode-extra-font-locking
    :straight t)
```


#### Cider ([repository](https://github.com/clojure-emacs/cider))

Provides integration with a Clojure repl.

```emacs-lisp
  (use-package cider
    :straight t
    :after company
    :config
    (setq cider-repl-history-file "~/.emacs.d/cider-history"
          cider-repl-use-clojure-font-lock t
          cider-repl-result-prefix ";; => "
          cider-repl-wrap-history t
          cider-repl-history-size 3000
          cider-show-error-buffer nil
          nrepl-hide-special-buffers t)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'subword-mode)
    (add-hook 'cider-repl-mode-hook #'enable-paredit-mode))
```


#### inf-clojure

`inf-clojure` is a third-party package offering basic integration with a running Clojure subprocess. This package is necessary for running a Figwheel process with Emacs. It's not as feature-rich as CIDER, but still offers the ability to load files, switch namespaces, evaluate expressions, show documentation, and do macro-expansion.

*Currently disabled due to conflicts with `cider`*

```emacs-lisp
  (use-package inf-clojure
    :straight t
    :init
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))
```

Now lets write a simple function to run Figwheel as a Clojure subprocess.

```emacs-lisp
   (defun figwheel-repl ()
     (interactive)
     (inf-clojure "lein figwheel"))
```


#### Linting Clojure

The [flycheck-clojure](https://github.com/clojure-emacs/squiggly-clojure) package allows syntax checking for Clojure(Script). It uses [eastwood](https://github.com/jonase/eastwood), [core.typed](http://typedclojure.org/) and [kibit](https://github.com/jonase/kibit) to lint Clojure(Script) through CIDER.

```emacs-lisp
  (use-package flycheck-clojure
    :straight t
    :after cider flycheck
    :config
    (flycheck-clojure-setup))
```

Okay. There's been some snares getting this package to work, but with the help of this [blogpost](http://blog.podsnap.com/squiggly.html) from the `flycheck-clojure` repo. (note to self: READMEs are friends), I'm beginning to make progress.

After cloning the project repo from my local file system, my debugging process has consisted of the following:

1.  Navigate to the [sample-project](https://github.com/clojure-emacs/squiggly-clojure/tree/master/sample-project) in the `squiggly-clojure` project repo.
2.  Open `core.clj`
3.  Launch an nrepl with Cider.
4.  See `flycheck-clojure` being weird.
5.  Annoyed Google search.
6.  Edit my `clojure` configuration based on the last blog post.
7.  Restart Emacs.
8.  Repeat.

After running into problems documented in issues [#45](https://github.com/clojure-emacs/squiggly-clojure/issues/45), [#13](https://github.com/clojure-emacs/squiggly-clojure/issues/13), and [#46,](https://github.com/clojure-emacs/squiggly-clojure/issues/46) ~~I finally was able to get `flycheck-clojure` to stop doing weird things~~.

~~It's now doing nothing at all.~~

The project maintainers provide an [example-config](https://github.com/clojure-emacs/example-config) for setting up emacs, cider, flycheck, and friends. I messed around with this config for about half an hour and the latency issues and general inconsistency are the same.

I'm guessing I need to actually include the linters in my project's `project.clj`, but it's weird this package worked at all for bits and stretches if the dependencies need to be installed manually.


#### Typed Clojure

Let's give this [guy](https://github.com/typedclojure/typed-clojure-mode) a try.

```emacs-lisp
  (use-package typed-clojure-mode
     :straight t
     :after clojure-mode
     :init
     (add-hook 'clojure-mode-hook 'typed-clojure-mode))
```

`java.lang.GoEFUrself`!

    # CompilerException java.lang.RuntimeException: Unable to resolve symbol: sym in this context, compiling:(clojure/core/types.clj:1170:5)

I've now stumbled upon `cider--debug-mode`.

This mode cannot be called manually, but with `C-u C-M-x` instead, and now `flycheck-clojure` ~~appears to be sort of working~~.

3 months later&#x2026;

The above strikethrough denotes the point in time where I officially said [FI](https://www.si.com/nba/2017/05/16/steve-kerr-sneakers-message).


#### Flycheck-Pos-Tip

The `flycheck-clojure` repository recommendeds to install [flycheck-pos-tip](https://github.com/flycheck/flycheck-pos-tip) to keep linting and type errors from clashing with CIDER eldoc information.

```emacs-lisp
  (use-package flycheck-pos-tip
    :straight t
    :after flycheck
    :init
    (flycheck-pos-tip-mode)
    :config
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages))
```


### Java


#### Eclim

```emacs-lisp
  (use-package eclim
    :straight t
    :if (eq system-type 'darwin)
    ;; load my forked version
    ;; :load-path "site-lisp/emacs-eclim/"
    :config
    (setq eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse")
          eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"
          eclimd-executable "/Applications/Eclipse.app/Contents/Eclipse/eclimd"
          eclimd-default-workspace "~/Developer/Projects/Java/Workspace"
          eclimd-autostart-with-default-workspace t
          eclim-autostart nil
          eclim-wait-for-process t))
```


### LaTeX


#### Tex Config

```emacs-lisp
  (use-package tex
    :ensure auctex
    :defines latex-nofill-env
    :functions chaffin/tex-auto-fill-mode
    :init
    (progn
      (setq TeX-command-default "LaTeX"
            TeX-engine 'xetex
            TeX-auto-save t
            TeX-parse-self t
            TeX-syntactic-comment t
            TeX-source-correlate-start-server nil
            LaTeX-fill-break-at-separators nil)
      (defvar latex-nofill-env '("equation"
                                 "equation*"
                                 "align"
                                 "align*"
                                 "tabular"
                                 "tikzpicture"))
      (defun chaffin//tex-autofill ()
        "Check whether the pointer is currently inside one of
  the environments in `latex-nofill-env` and inhibits auto-filling
  of the current paragraph."
        (let ((do-auto-fill t)
              (current-environment "")
              (level 0))
          (while (and do-auto-fill (not (string- current-environment "document")))
            (setq level (1+ level)
                  current-environment (LaTeX-current-environment level)
                  do-auto-fill (not (member current-environment latex-nofill-env)))))
        (when do-auto-fill
          (do-auto-fill)))

      (defun chaffin/tex-auto-fill-mode ()
        (interactive)
        (auto-fill-mode)
        (setq auto-fill-mode 'chaffin/tex-autofill))

      (add-hook 'LaTeX-mode-hook 'chaffin/tex-auto-fill-mode)
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
      (add-hook 'LaTeX-mode-hook 'smartparens-mode)
      ;; Company AucTeX
        (use-package company-auctex
          :straight t
          :init
          (company-auctex-init))
      ;; RefTeX
        (defun jchaffin/init-reftex ()
          (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
          (setq reftex-plug-into-AUCTeX '(nil nil t t t)
                reftex-use-fonts t
                reftex-default-bibliography '("~/Dropbox/org/papers/references.bib")))
      (jchaffin/init-reftex)
      ;; Bibtex Config
      
      ;; Magic Latex Buffer
        (use-package magic-latex-buffer
          :straight t
          :init
          (progn
            (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
            (setq magic-latex-enable-block-highlight t
                  magic-latex-enable-suscript t
                  magic-latex-enable-pretty-symbols t
                  magic-latex-enable-block-align t
                  magic-latex-enable-inline-image t)))
      ;; Latex Preview Pane
      ;; buggy
      ;;   (use-package latex-preview-pane
      ;;     :straight t
      ;;     :config (latex-preview-pane-enable))

      ;; Org Edit Latex Buffer
        (use-package org-edit-latex
          :straight t)
      ;; Latex Extra
      (use-package latex-extra
        :straight t
        :init
        (add-hook 'LaTeX-mode-hook 'latex-extra-mode))
      ;; Auctex latexmk
      (use-package auctex-latexmk
        :straight t
        :config
        (setq auctex-latexmk-inherit-TeX-PDF-mode t))
      ))
```


#### Bibtex

\#+NAME bibtex-config

```emacs-lisp
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)
```


#### LaTeX Extra

```emacs-lisp
(use-package latex-extra
  :straight t
  :init
  (add-hook 'LaTeX-mode-hook 'latex-extra-mode))
```


#### LaTeX Preview Pane

```emacs-lisp
  (use-package latex-preview-pane
    :straight t
    :config (latex-preview-pane-enable))
```


#### Company AucTeX

```emacs-lisp
  (use-package company-auctex
    :straight t
    :init
    (company-auctex-init))
```


#### Org Edit Latex

```emacs-lisp
  (use-package org-edit-latex
    :straight t)
```


#### Magic Latex Buffer

Prettify dedicated org-mode latex buffers.

```emacs-lisp
  (use-package magic-latex-buffer
    :straight t
    :init
    (progn
      (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
      (setq magic-latex-enable-block-highlight t
            magic-latex-enable-suscript t
            magic-latex-enable-pretty-symbols t
            magic-latex-enable-block-align t
            magic-latex-enable-inline-image t)))
```


#### Auctex Latexmk

```emacs-lisp
(use-package auctex-latexmk
  :straight t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))
```


#### RefTeX

RefTeX is a citation and reference tool maintained by the AucTeX team.

Since Emacs 24.3, its built in with the Emacs distribution.

```emacs-lisp
  (defun jchaffin/init-reftex ()
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX '(nil nil t t t)
          reftex-use-fonts t
          reftex-default-bibliography '("~/Dropbox/org/papers/references.bib")))
```


#### Texinfo

\#+texinfo-config

```emacs-lisp
  (use-package texinfo
    :straight t
    :defines texinfo-section-list
    :commands texinfo-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))
```


### Javascript


#### JavaScript Preamble

```emacs-lisp
    (use-package js2-mode
      :straight t
      :mode (("\\.js\\'" . js2-mode))
      :config
      (setq js-indent-level 2))
    (use-package coffee-mode
      :straight t
      :mode ("\\.coffee\\'" . coffee-mode))
    (use-package json-mode
      :defer t
      :straight t
      :mode (("\\.json\\'" . json-mode)))
    (use-package tern
      :straight t
      :after js2-mode
      :init (add-hook 'js2-mode-hook 'tern-mode))
```


#### js2-mode

```emacs-lisp
  (use-package js2-mode
    :straight t
    :mode (("\\.js\\'" . js2-mode))
    :config
    (setq js-indent-level 2))
```


#### rjsx-mode

Real jsx support.

```emacs-lisp
  (use-package rjsx-mode
    :straight t
    :mode "\\.jsx\\'")
```


#### Tern

[Tern](https://github.com/proofit404/company-tern) is a code-analysis engine for JavaScript.

```emacs-lisp
  (use-package tern
    :straight t
    :after js2-mode
    :init (add-hook 'js2-mode-hook 'tern-mode))
```


##### Company Tern ([repository](https://github.com/proofit404/company-tern))

Tern backend using company.

```emacs-lisp
  (use-package company-tern
    :straight t
    :init
    (add-to-list 'company-backends 'company-tern)
    :config
    (setq company-tern-property-marker nil
          company-tern-meta-as-single-line t))
```


#### JSON

```emacs-lisp
  (use-package json-mode
    :defer t
    :straight t
    :mode (("\\.json\\'" . json-mode)))
```

Based off/shamelessly copied and pasted from Spacemacs React layer.

```emacs-lisp
  (progn
    (define-derived-mode react-mode web-mode "react")
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
    (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
    (add-to-list 'auto-mode-alist '("\\.index.android.js\\'" . react-mode))
    (add-to-list 'auto-mode-alist '("\\.index.ios.js\\'" . react-mode))
    (add-to-list 'auto-mode-alist '("\\/\\*\\* @jsx .*\\*/\\'" . react-mode)))
```


#### Coffee

```emacs-lisp
  (use-package coffee-mode
    :straight t
    :mode ("\\.coffee\\'" . coffee-mode))
```


#### Add Node Modules Path ([repository](https://github.com/codesuki/add-node-modules-path))

```emacs-lisp
  (use-package add-node-modules-path
    :straight t
    :defer t
    :init
    (add-hook 'js2-mode-hook #'add-node-modules-path))
```


#### Npm Mode

```emacs-lisp
  (use-package npm-mode
    :straight t
    :defer t)
```


#### Enable syntax checking in `js-mode` and related mode buffers.

```emacs-lisp
  (dolist (mode '(coffee-mode js2-mode json-mode))
    (push mode flycheck-global-modes))
```


### Web


#### Web Mode

```emacs-lisp
  (use-package web-mode
    :straight t
    :bind (:map web-mode-map
                ("M-n" . web-mode-tag-match))
    :mode
    (("\\.phtml\\'"      . web-mode)
     ("\\.tpl\\.php\\'"  . web-mode)
     ("\\.twig\\'"       . web-mode)
     ("\\.html\\'"       . web-mode)
     ("\\.htm\\'"        . web-mode)
     ("\\.[gj]sp\\'"     . web-mode)
     ("\\.as[cp]x?\\'"   . web-mode)
     ("\\.eex\\'"        . web-mode)
     ("\\.erb\\'"        . web-mode)
     ("\\.mustache\\'"   . web-mode)
     ("\\.handlebars\\'" . web-mode)
     ("\\.hbs\\'"        . web-mode)
     ("\\.eco\\'"        . web-mode)
     ("\\.ejs\\'"        . web-mode)
     ("\\.djhtml\\'"     . web-mode))

    :config
    (progn
      (setq web-mode-engines-alist
          '(("php" . "\\.phtml\\'")
            ("blade" . "\\.blade\\'")))

      (defun jchaffin/web-mode-enable ()
        (setq web-mode-enable-auto-pairing t
              web-mode-enable-css-colorization t
              web-mode-enable-block-face t
              web-mode-enable-part-face t
              web-mode-enable-comment-keywords t
              web-mode-enable-heredoc-fontification t
              web-mode-enable-current-element-highlight t
              web-mode-enable-current-column-highlight t))

      (add-hook 'web-mode-hook #'jchaffin/web-mode-enable)


      (defun jchaffin/web-mode-indent ()
        (setq web-mode-markup-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 1
              web-mode-script-padding 1
              web-mode-block-padding 0
              web-mode-comment-style 2))

      (add-hook 'web-mode-hook #'jchaffin/web-mode-indent)

        (use-package tagedit
          :straight t
          :diminish tagedit-mode
          :config
          (progn
            (tagedit-add-experimental-features)
            (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
      
        (use-package emmet-mode
          :straight t
          :defer t
          :init
          (add-to-hooks 'emmet-mode '(css-mode-hook
                                     html-mode-hook
                                     web-mode-hook)))
        (use-package company-web
          :straight t
          :init
          (progn
              (use-package company-tern
                :straight t
                :init
                (add-to-list 'company-backends 'company-tern)
                :config
                (setq company-tern-property-marker nil
                      company-tern-meta-as-single-line t))
            (defun jchaffin/company-web-mode-hook ()
              "Autocompletion hook for web-mode"
              (set (make-local-variable 'company-backends)
                   '(company-tern company-web-html company-yasnippet company-files)))
      
            (add-hook 'web-mode-hook #'jchaffin/company-web-mode-hook)
      
            ;; Enable JavaScript completion between <script>...</script> etc.
            (defadvice company-tern (before web-mode-set-up-ac-sources activate)
              "Set `tern-mode' based on current language before running company-tern."
              (if (equal major-mode 'web-mode)
                  (let ((web-mode-cur-language
                         (web-mode-language-at-pos)))
                    (if (or (string= web-mode-cur-language "javascript")
                            (string= web-mode-cur-language "jsx"))
                        (unless tern-mode (tern-mode))
                      (if tern-mode (tern-mode -1))))))))))
```


#### Emmet

```emacs-lisp
  (use-package emmet-mode
    :straight t
    :defer t
    :init
    (add-to-hooks 'emmet-mode '(css-mode-hook
                               html-mode-hook
                               web-mode-hook)))
```


#### HTML


##### Tag Edit

```emacs-lisp
  (use-package tagedit
    :straight t
    :diminish tagedit-mode
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
```


#### CSS


##### CSS Mode

```emacs-lisp
  (use-package css-mode
    :straight t
    :defer t
    :commands css-expand-statment css-contract-statement
    :bind (("C-c c z" . css-contract-statement)
           ("C-c c o" . css-expand-statement))
    :init
    (progn
      (defun css-expand-statment ()
        (interactive)
        (save-excursion
          (end-of-line)
          (search-backward "{")
          (forward-char 1)
          (while (or (eobp) (not (looking-at "}")))
            (let ((beg (point)))
              (newline)
              (search-forward ";")
              (indent-region beg (point))))
          (newline)))

      (defun css-contrac-statement ()
        "Contract CSS Block"
        (interactive)
        (end-of-line)
        (search-backward "{")
        (while (not (looking-at "}"))
          (join-line -1)))))
```


##### Less

```emacs-lisp
  (use-package css-less-mode
    :straight t
    :mode ("\\.less\\'" . less-css-mode))
```


##### SASS

```emacs-lisp
  (use-package sass-mode
    :straight t
    :mode ("\\.sass\\'" . sass-mode))
```


##### SCSS

```emacs-lisp
  (use-package scss-mode
    :straight t
    :mode ("\\.scss\\'" . scss-mode))
```


#### Syntax Checking and Code Completion


##### Company Web ([repository](https://github.com/osv/company-web))

Code completion for html-mode, web-mode, jade-mode, and slim-mode using company.

```emacs-lisp
  (use-package company-web
    :straight t
    :init
    (progn
        (use-package company-tern
          :straight t
          :init
          (add-to-list 'company-backends 'company-tern)
          :config
          (setq company-tern-property-marker nil
                company-tern-meta-as-single-line t))
      (defun jchaffin/company-web-mode-hook ()
        "Autocompletion hook for web-mode"
        (set (make-local-variable 'company-backends)
             '(company-tern company-web-html company-yasnippet company-files)))

      (add-hook 'web-mode-hook #'jchaffin/company-web-mode-hook)

      ;; Enable JavaScript completion between <script>...</script> etc.
      (defadvice company-tern (before web-mode-set-up-ac-sources activate)
        "Set `tern-mode' based on current language before running company-tern."
        (if (equal major-mode 'web-mode)
            (let ((web-mode-cur-language
                   (web-mode-language-at-pos)))
              (if (or (string= web-mode-cur-language "javascript")
                      (string= web-mode-cur-language "jsx"))
                  (unless tern-mode (tern-mode))
                (if tern-mode (tern-mode -1))))))))
```


### Markdown


#### Markdown Mode

-   See <https://jblevins.org/projects/markdown-mode/>

\#+NAME markdown-mode-config

```emacs-lisp
  (use-package markdown-mode
    :straight t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init
    (progn
      (setq markdown-command "multimarkdown")
      (when *is-mac*
        ;; FIX ME
        (setq markdown-open-command (lambda () (shell-command "open -a Marked" buffer-file-name))))))
```


#### markdown-mode+

```emacs-lisp
  (use-package markdown-mode+
    :if *is-mac*
    :straight t)
```


### Ruby


#### ruby-mode

```emacs-lisp
  (use-package ruby-mode
    :mode "\\.rb\\'"
    :interpreter "ruby"
    :functions inf-ruby-keys
    :config
    (defun chaffin/ruby-mode-hook ()
      (require 'inf-ruby)
      (inf-ruby-keys))

    (add-hook #'ruby-mode-hook #'chaffin/ruby-mode-hook))
```


### Docker

```emacs-lisp
  (use-package dockerfile-mode
    :straight t
    :mode ( "Dockerfile\\'" .  dockerfile-mode))
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> [Emacs Manual - C.4.1 General Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/General-Variables.html#General-Variables)

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> [Emacs Manual - 10.2 Conditionals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html)

<sup><a id="fn.3" class="footnum" href="#fnr.3">3</a></sup> [EmacsForMacOS](https://www.emacswiki.org/emacs/EmacsForMacOS)

<sup><a id="fn.4" class="footnum" href="#fnr.4">4</a></sup> [Quora - Is Windows POSIX compliant?](https://www.quora.com/Is-Windows-POSIX-compliant)

<sup><a id="fn.5" class="footnum" href="#fnr.5">5</a></sup> [Emacs Manual - 51.1.4 Saving Customizations](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html)

<sup><a id="fn.6" class="footnum" href="#fnr.6">6</a></sup> See [Note taking with pdf-tools](https://matt.hackinghistory.ca/2015/11/11/note-taking-with-pdf-tools/)

<sup><a id="fn.7" class="footnum" href="#fnr.7">7</a></sup> [Mastering Emacs - Keeping Secrets in Emacs GnuPG Auth Sources](https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources)

<sup><a id="fn.8" class="footnum" href="#fnr.8">8</a></sup> [Emacs Wiki - Tabs Are Evil](https://www.emacswiki.org/emacs/TabsAreEvil)
