;; org-dotemacs: code extracted from ~/.emacs.d/dotemacs.org
;; Block = @10248
(message "org-dotemacs: evaluating @10248 block")
(use-package page-break-lines
  :init
  (global-page-break-lines-mode))
;; Block = @10409
(message "org-dotemacs: evaluating @10409 block")
(use-package dash
  :config
  (dash-enable-font-lock))
;; Block = @10545
(message "org-dotemacs: evaluating @10545 block")
(use-package dash-functional)
;; Block = @10638
(message "org-dotemacs: evaluating @10638 block")
(use-package cl-lib)
;; Block = @10734
(message "org-dotemacs: evaluating @10734 block")
(use-package cl-lib-highlight
  :after (cl-lib)
  :demand t)

;; Block = @10898
(message "org-dotemacs: evaluating @10898 block")
(use-package f)
;; Block = @11007
(message "org-dotemacs: evaluating @11007 block")
(use-package s)
;; Block = @11082
(message "org-dotemacs: evaluating @11082 block")
(use-package a)
;; Block = @11290
(message "org-dotemacs: evaluating @11290 block")
(use-package el-patch
    :init
  (defun el-patch-remove-feature ()
    (interactive)
    (let ((feature (completing-read "Feature: " el-patch-pre-validate-hook))
          (patch (call-interactively #'el-patch-unpatch)))
      (remove-hook 'el-patch-pre-validate-hook (intern feature))
      (remhash patch el-patch--patches))))

;; Block = @11667
(message "org-dotemacs: evaluating @11667 block")
(use-package async
  :after dired
  :commands (dired-async-mode async-smtpmail-send-it)
  :init
  (with-eval-after-load 'async
    (require 'smtpmail-async)
    (setq message-send-mail-function 'async-smtpmail-send-it)
    (dired-async-mode 1)))
;; Block = @11958
(message "org-dotemacs: evaluating @11958 block")
(use-package esup)
;; Block = @12053
(message "org-dotemacs: evaluating @12053 block")
(setq initial-scratch-message nil
      initial-major-mode 'org-mode
      inhibit-startup-echo-area-message t)
;; Block = @12616
(message "org-dotemacs: evaluating @12616 block")
(use-package dashboard
  :init
  (defun halidom/dashboard-banner ()
      "Set a dashboard banner including information on
  package initialization time and garbage collections."
      (setq dashboard-banner-logo-title
            (format
             (concat "Emacs ready in %.2f seconds "
                       "with %d garbage collections.")
             (float-time (time-subtract
                          after-init-time
                          before-init-time))
             gcs-done)))


    (unless global-page-break-lines-mode
      (global-page-break-lines-mode))

    (dashboard-setup-startup-hook)

    :config
    (add-to-list 'dashboard-items '(agenda) t)
    ;; Get the week agenda.
    ;; See `dashboard-get-agenda' for how this
    ;; variable is being used.
    (setq show-week-agenda-p t)

    (setq dashboard-items '(( agenda . 10)
                            ( projects . 5)
                            ( recents . 3)
                            ( bookmarks . 5)))

    :hook
    (after-init     . dashboard-refresh-buffer)
    (dashboard-mode . halidom/dashboard-banner))


;; Block = @13817
(message "org-dotemacs: evaluating @13817 block")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
;; Block = @14457
(message "org-dotemacs: evaluating @14457 block")
(use-package ns-auto-titlebar
  :init
  (when (eq system-type 'darwin)
    (ns-auto-titlebar-mode)))
;; Block = @14733
(message "org-dotemacs: evaluating @14733 block")
(setq-default frame-title-format "%b")

;; Block = @15099
(message "org-dotemacs: evaluating @15099 block")
(setq user-full-name "Jacob Chaffin"
      user-mail-address "jchaffin@ucla.edu")
;; Block = @15265
(message "org-dotemacs: evaluating @15265 block")
;; Customization Group
(defgroup halidom nil
  "Customization group for the `halidom' Emacs configuration."
  :group 'applications
  :prefix "halidom-")

(defcustom halidom-prefix "\M-m"
  "The prefix map leader key.")
;; Block = @16467
(message "org-dotemacs: evaluating @16467 block")
(defun rev-up-gc ()
   (interactive)
  (setq gc-cons-threshold most-positive-fixnum))

(defun rev-down-gc ()
   (interactive)
  (setq gc-cons-threshold 800000))


;; Block = @16723
(message "org-dotemacs: evaluating @16723 block")
(add-hook 'minibuffer-setup-hook #'rev-up-gc)
(add-hook 'minibuffer-exit-hook #'rev-down-gc)
;; Block = @16883
(message "org-dotemacs: evaluating @16883 block")

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("o" "~" ".lbin" ".so" ".a"
                ".git/" ".hg/" ".svn" ".svn-base")))
;; Block = @17120
(message "org-dotemacs: evaluating @17120 block")
(setq load-prefer-newer t)
;; Block = @17203
(message "org-dotemacs: evaluating @17203 block")
(defalias 'yes-or-no-p 'y-or-n-p)
;; Block = @17356
(message "org-dotemacs: evaluating @17356 block")
(setq-default fill-column 80)
;; Block = @17506
(message "org-dotemacs: evaluating @17506 block")
(setq system-uses-terminfo t)
;; Block = @17779
(message "org-dotemacs: evaluating @17779 block")
(use-package restart-emacs)
;; Block = @17923
(message "org-dotemacs: evaluating @17923 block")

(defun straight-reload-init (&optional debug)
  "Reload init file using straight transaction system."
  (interactive "P")
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading initialization file...")
    (when debug
      (set (make-local-variable 'use-literate-p) nil)
      (set (make-local-variable 'user-init-file)
	   (expand-file-name "init.el" user-emacs-directory)))
    (load user-init-file nil 'nomessage)
    (message "Reloading initialization file...done.")))
;; Block = @18566
(message "org-dotemacs: evaluating @18566 block")
(defun straight-eval-buffer ()
  "Evaluate current buffer using the straight transaction system."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (or (null buffer-file-name)
           (not (file-exists-p buffer-file-name)))
        (eval-buffer)
      (progn
        (when (string= buffer-file-name user-init-file)
          (straight-mark-transaction-as-init))
        (load-file buffer-file-name))))
  (message "Evaluating %s...done." (buffer-name)))

;; Block = @19119
(message "org-dotemacs: evaluating @19119 block")
(use-package eval-in-repl)
;; Block = @19280
(message "org-dotemacs: evaluating @19280 block")
(defmacro if-not (condition then-form &rest rest-forms)
  (declare (indent 2))
  `(progn
     (if (not ,condition)
	       ,then-form
       ,@rest-forms)))
;; Block = @19526
(message "org-dotemacs: evaluating @19526 block")
(defmacro with-major-mode (mode &rest body)
  "If the current major-mode is MODE, then execute BODY."
  (declare (indent defun))
  `(when (equal major-mode ',mode)
     ,@body))
;; Block = @19790
(message "org-dotemacs: evaluating @19790 block")
(defmacro if-major-mode (mode then-form &rest rest-forms)
  "If MODE, then execute THEN-FORM, else execute REST-FORMS."
  (declare (indent defun))
  `(progn
     (if (equal major-mode ',mode)
	       ,then-form
       ,@rest-forms)))
;; Block = @20121
(message "org-dotemacs: evaluating @20121 block")
(defun list-enabled-minor-modes (&optional buf)
  "The minor modes enabled in the current buffer."
  (let ((auto-save-mode nil)
        (buf (or buf (current-buffer))))
    (cl-loop for mode being the element of minor-mode-list
             when (boundp mode)
             when (symbol-value mode)
             collect mode)))


(defvar minor-modes-enabled-list (list-enabled-minor-modes (current-buffer))
  "The list of enabled minor modes")

(defun minor-mode-enabled-p (mode)
  (member mode (list-enabled-minor-modes (current-buffer))))
;; Block = @20850
(message "org-dotemacs: evaluating @20850 block")

(defun unadvise (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; Block = @21129
(message "org-dotemacs: evaluating @21129 block")

(defun buffer-list-names ()
  "Get list of buffer names."
  (let ((f (lambda (b) (buffer-name b)))
	      (buffer-alist (buffer-list)))
    (mapcar f buffer-alist)))

;; Block = @21383
(message "org-dotemacs: evaluating @21383 block")

(defun scratch (&optional new)
  "Switch to scratch buffer. If optional prefix NEW,
then create a new buffer. Else reuse the existing scratch buffer,
generating a new one if the initial scratch buffer has been killed."
  (interactive "P")
  (unless (or new (not (seq-contains (buffer-list) (get-buffer "*scratch*"))))
    (with-current-buffer (generate-new-buffer "*scratch*")
      (emacs-lisp-mode)))
  (switch-to-buffer-other-window "*scratch*"))


;; Block = @21947
(message "org-dotemacs: evaluating @21947 block")
(use-package bui)
;; Block = @22057
(message "org-dotemacs: evaluating @22057 block")
(defun window-count ()
  "Count number of windows in the current frame."
  (interactive)
  (length (window-list)))
;; Block = @22257
(message "org-dotemacs: evaluating @22257 block")
(defun window-count-unique ()
  "Count number of unique windows in the current frame"
  (interactive)
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list)))))
;; Block = @22513
(message "org-dotemacs: evaluating @22513 block")
(defun window-buffer-list ()
  "Get list of buffers in an open window."
  (let ((windows))
    (dolist (frame (frame-list) windows)
      (with-selected-frame frame
      (setq windows (append (window-list) windows))))
        (map 'seq-uniq (lambda (w) (window-buffer w)) windows)))
;; Block = @22858
(message "org-dotemacs: evaluating @22858 block")
(defun buffer-list-modes ()
  "Restart org-mode in all org buffers in open windows."
  (let ((modes))
    (dolist (buf (window-buffer-list) modes)
      (with-current-buffer buf
        (setq modes (push major-mode modes))))
    (seq-uniq modes)))
;; Block = @24033
(message "org-dotemacs: evaluating @24033 block")
(defun basename (pathname)
  "Return the filename or directory portion of PATHNAME"
  (if (or (file-directory-p pathname)
          (string-match "/$" pathname))
      (let ((dirname (directory-file-name pathname)))
        (file-name-nondirectory dirname))
    (file-name-nondirectory pathname)))
;; Block = @24402
(message "org-dotemacs: evaluating @24402 block")
(defun file-path ()
  (destructuring-bind (file dir)
      (cond ((eq major-mode 'dired-mode)
             (list (substring-no-properties (thing-at-point 'symbol))
                   dired-directory))
            ((stringp buffer-file-name)
             (mapcar (lambda (f) (funcall f buffer-file-name))
                     '(file-name-nondirectory file-name-directory)))
            (t  (list (buffer-name (current-buffer)) default-directory)))
    (expand-file-name file dir)))

(defun copy-file-path-as-kill ()
  "Copies the file path and applies the result as an argument to
function FUNC. To copy the file path to the kill-ring, use the
 interactive function `copy-file-path-as-kill'."
  (interactive)
  (let ((path (file-path)))
    (kill-new path)
    (message "Copied %s" path)))

;; Block = @25299
(message "org-dotemacs: evaluating @25299 block")
(defun directory-files-no-wildcards (directory &optional full nosort)
   "List directory contents without wildcards"
   (cddr (directory-files directory full nil nosort)))
;; Block = @25653
(message "org-dotemacs: evaluating @25653 block")
(defun read-file-contents (file)
  "Return contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))
;; Block = @25871
(message "org-dotemacs: evaluating @25871 block")
(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))
;; Block = @26105
(message "org-dotemacs: evaluating @26105 block")

(defun resolve-path (&rest paths)
  "Concatenate path segments."
  (let ((paths- (mapcar #'directory-file-name paths)))
    (mapconcat 'identity paths- "/")))

;; Block = @26360
(message "org-dotemacs: evaluating @26360 block")
(cl-defun user-home (&rest path-segments &key (slash nil) &allow-other-keys)
  "Resolves the absolute path formed PATH-SEGMENTS to the
   user home directory. If the optional argument SLASH is supplied,
the the returned file path will be formatted as a directory. "
  (let ((segments (seq-filter #'stringp path-segments)))
    (--> (getenv "HOME")
         (f-split it)
         (append it segments)
         (cdr it)
         (cons (concat "/" (car it)) (cdr it))
         (apply #'resolve-path it)
         (if slash (file-name-as-directory it) it))))
;; Block = @26992
(message "org-dotemacs: evaluating @26992 block")
(defalias #'dropbox-dir (apply-partially #'user-home "Dropbox"))
;; Block = @27136
(message "org-dotemacs: evaluating @27136 block")
(defalias #'projects-dir
 (apply-partially #'user-home "Developer" "Projects"))
;; Block = @27291
(message "org-dotemacs: evaluating @27291 block")
(defalias #'emacs-dir (apply-partially #'user-home ".emacs.d")
  "Resolve PATH-SEGMENTS to `user-emacs-directory'.")
;; no littering directories
(defalias #'emacs-var-dir
  (apply-partially #'emacs-dir "var")
  "Resolve PATH-SEGMENTS to `no-littering-var-directory.'")
(defalias #'emacs-etc-dir
  (apply-partially #'emacs-dir "etc")
  "Resolve PATH-SEGMENTS to `no-littering-etc-directory.'")
;; straight.el directories
(defalias #'straight-dir
  (apply-partially #'emacs-dir "straight")
  "Resolve PATH-SEGMENTS to straight.el installation directory.")
(defalias #'straight-repos-dir
  (apply-partially #'straight-dir "repos")
  "Resolve PATH-SEGMENTS to straight.el repos directory.")
(defalias #'straight-build-dir
  (apply-partially #'straight-dir "build")
  "Resolve PATH-SEGMENTS to straight.el build directory")
;; org directories
(defalias #'org-dir
  (apply-partially #'dropbox-dir "org")
  "Resolve PATH-SEGMENTS to `org-directory'.")
(defalias #'agenda-dir
  (apply-partially #'org-dir "agenda")
  "Resolve PATH-SEGMENTs to directory of agenda files.")
;; Block = @28426
(message "org-dotemacs: evaluating @28426 block")
(defcustom read-only-directories '()
  "A list of directories for which all files and subdirectories
should open in `read-only-mode'."
  :type '(repeat :tag "List of blacklisted directories" file)
  :group 'files)

(defcustom read-only-whitelist-directories '()
  "A list of directories which should not be opened in read-only mode."
  :type '(repeat :tag "List of blacklisted directories" file)
  :group 'files)

(defvar read-only-file-extensions '("\\.gz\\'")
  "Not yet implemented.")

(defun dir-level (dir)
  (length (f-split dir)))

(defun dir-or-subdir-p (d1 d2)
  "If non-nil, then directory D1 is subdirectory or the same directory as D2."
  (cond ((< (dir-level d1) (dir-level d2)) nil)
        ((string= d1 d2) t)
        (t (dir-or-subdir-p
            (--> d1 basename file-name-as-directory (string-remove-suffix it d1))
            d2))))

(defun read-only-directory-p (bf)
"Determine whether the current buffer file is in a directory
that is a member of `read-only-directories'."
  (cl-flet ((f (d) (funcall #'dir-or-subdir-p bf d)))
    (and (not (null (seq-filter #'f read-only-directories)))
         (null (seq-filter #'f read-only-whitelist-directories))
         (file-writable-p bf))))

(defun halidom/open-buffer-as-read-only (file)
  "All buffers from `read-only-directories' or
`read-only-file-extensions' are set to read-only."
  (let ((bd (and buffer-file-name (file-name-directory file))))
    (if (read-only-directory-p bd) (read-only-mode 1))))

(defun halidom/find-file-read-only-hook ()
  (funcall #'halidom/open-buffer-as-read-only (buffer-file-name)))

(setq read-only-directories `(,(straight-dir :slash t) "/usr/local/"))

(setq read-only-whitelist-directories '("/usr/local/src/llvm-project/"))
(add-hook 'find-file-hook #'halidom/find-file-read-only-hook)
;; Block = @30434
(message "org-dotemacs: evaluating @30434 block")
(defun straight-installed-packages ()
  (--> straight--recipe-cache
       (hash-table-keys it)
       (seq-difference
        it
        (mapcar
         #'symbol-name
         straight-built-in-pseudo-packages))
       (sort it #'string-lessp)))

(defun straight-package-installed-p (pkg)
  (member (symbol-name pkg) (straight-installed-packages)))

(cl-defun straight-browse-local (&optional build-dir)
  "Go to a straight repository directory. If BUILD-DIR, then go to
  the build directory for that repository instead."
  (interactive "P")
  (lexical-let* ((dir (-> user-emacs-directory
                         (f-join "straight"
                                 (if build-dir "build" "repos"))))

                 (msg (format "(%s) Goto recipe: "
                              (upcase-initials (f-base dir))))
                 (pkg-keys (straight-installed-packages)))
    (ivy-read
     msg
     pkg-keys
     :action
     (lambda (package)
       (lexical-let (pkg-directory pkg-file)
         (condition-case nil
             (if (and build-dir
                    (not (plist-get
                        (gethash package straight--recipe-cache)
                        :no-build)))
                 (progn
                   (setq pkg-directory (expand-file-name package dir))
                   (and (file-directory-p pkg-directory)
                      (dired pkg-directory)))
               (let ((repo (plist-get
                            (gethash package straight--recipe-cache)
                            :local-repo)))
                 (if repo
                     (setq pkg-directory
                           (expand-file-name
                            repo
                            (replace-regexp-in-string "build" "repos" dir))
                           pkg-file
                           (car
                            (directory-files
                             pkg-directory t
                             (concat "\\README.*\\'\\|" package ".el"))))
                   (setq pkg-directory
                         (file-name-directory (locate-library package))
                         pkg-file
                         (car
                          (directory-files
                           pkg-directory t
                           (concat package ".el\\(?:.gz\\)")))))
                 (if pkg-file
                     (and (file-exists-p pkg-file)
                        (find-file pkg-file))
                   (and (file-directory-p pkg-directory)
                      (dired pkg-directory)))))))))))

(define-key goto-map "r" #'straight-browse-local)
;; Block = @33120
(message "org-dotemacs: evaluating @33120 block")
(defun straight-browse-remote (&optional package)
  "View a recipe PACKAGE on GitHub."
  (interactive "P")
  (cl-flet ((remote-url (pkg)
              (let ((recipe (cdr (straight-recipes-retrieve pkg))))
                (destructuring-bind (repo host)
                    `(,(plist-get recipe :repo)
                      ,(plist-get recipe :host))
                  (if (eq host 'github)
                      (concat "https://github.com/" repo)
                    (message "%s is not a GitHub repository." pkg))))))
    (let* ((pkg (if (interactive-p)
                    (completing-read
                     "Which recipe? "
                     (straight-recipes-list straight-recipe-repositories)
                     nil 'require-match)))
           (url (remote-url (intern pkg))))
      (browse-url url))))
;; Block = @34573
(message "org-dotemacs: evaluating @34573 block")
(setq gnutls-min-prime-bits 4096)
;; Block = @34704
(message "org-dotemacs: evaluating @34704 block")
(when (eq system-type 'darwin)
  (let* ((has-brew (not (string-empty-p
			 (shell-command-to-string
			  "which brew"))))
	 (gpg-path (if has-brew
		       (shell-command-to-string "brew --prefix gpg2")))
	 (has-gpg2 (if gpg-path
		             (file-exists-p
                  (replace-regexp-in-string "\n" "" gpg-path)))))
    (setq epg-gpg-program (if has-gpg2 "gpg2" "gpg"))))
;; Block = @35417
(message "org-dotemacs: evaluating @35417 block")
(setenv "GPG_AGENT_INFO" nil)
;; Block = @36124
(message "org-dotemacs: evaluating @36124 block")
(when (and (executable-find "gpg") (eq system-type 'darwin))
  (if-not (string-empty-p
       (shell-command-to-string
	      (concat "gpg --list-keys | grep " user-mail-address)))
      (progn
        (add-to-list 'load-path (emacs-etc-dir "secrets"))
        (require 'secrets))
    (print (format "GPG key(s) for %s not found"
                   (or user-full-name user-mail-address)))))
;; Block = @36660
(message "org-dotemacs: evaluating @36660 block")
(use-package hydra
  :init
  (eval-and-compile

    (defhydra hydra-reload-eval (:exit t :hint nil)

      "
  _i_nit   | _r_egion  | _d_efun
  -------^^+---------^^+--------
  _b_uffer | _R_estart | _q_uit
  -------^^+---------^^+--------"
      ("i" straight-reload-init)
      ("b" straight-eval-buffer)
      ("r" eval-region)
      ("d" eval-defun)
      ("R" restart-emacs)
      ("q" nil))

    (defhydra hydra-straight (:hint nil :exit t)

      "
  _c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
  _C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
  ----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
  _r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
  _R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |pru_n_e build
  ----------------^^+--------------^^+---------------^^+----------------^^+------------
  _b_rowse package local
  _B_rowse package remote
  "
      ("b" straight-browse-local)
      ("B" straight-browse-remote)
      ("c" straight-check-all)
      ("C" straight-check-package)
      ("r" straight-rebuild-all)
      ("R" straight-rebuild-package)
      ("f" straight-fetch-all)
      ("F" straight-fetch-package)
      ("p" straight-pull-all)
      ("P" straight-pull-package)
      ("m" straight-merge-all)
      ("M" straight-merge-package)
      ("n" straight-normalize-all)
      ("N" straight-normalize-package)
      ("u" straight-push-all)
      ("U" straight-push-package)
      ("v" straight-freeze-versions)
      ("V" straight-thaw-versions)
      ("w" straight-watcher-start)
      ("W" straight-watcher-quit)
      ("g" straight-get-recipe)
      ("n" straight-prune-build)
      ("q" nil))))
;; Block = @38609
(message "org-dotemacs: evaluating @38609 block")
(use-package ivy-hydra
  :after (hydra)
  :demand t)
;; Block = @38722
(message "org-dotemacs: evaluating @38722 block")
(use-package evil)
;; Block = @38811
(message "org-dotemacs: evaluating @38811 block")
(use-package which-key
  :custom
  (which-key-enable-extended-define-key t)
  (which-key-allow-multiple-replacements t)
  (which-key-compute-remaps t)
  (which-key-separator " → " )
  :init
  (which-key-mode 1))

;; Block = @39253
(message "org-dotemacs: evaluating @39253 block")
(use-package speed-type)
;; Block = @39389
(message "org-dotemacs: evaluating @39389 block")
(use-package evil)
;; Block = @40067
(message "org-dotemacs: evaluating @40067 block")
(use-package exec-path-from-shell
  ;; only load `exec-path-from-shell' package on macos and linux.
  :if (memq window-system '(mac ns))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))
;; Block = @40482
(message "org-dotemacs: evaluating @40482 block")

(use-package system-packages
  :init
  (with-eval-after-load 'cl-lib
    (defun system-packages/update-brew-commands (commands)
      "Update the brew commands supported in system-packages."
      (let ((brew-commands-alist
             (->> system-packages-supported-package-managers
                (assoc 'brew)
                cdr)))
        (dolist (command commands)
          (cl-destructuring-bind (cmd . cmd-string) command
            (setf (cdr (assoc cmd brew-commands-alist)) cmd-string))))))

  :config
  (with-eval-after-load 'system-packages
    (let ((commands-alist '((get-info . "brew info")
                            (verify-all-packages . "brew doctor")
                            (log . "brew log"))))
      (system-packages/update-brew-commands commands-alist))))
;; Block = @41347
(message "org-dotemacs: evaluating @41347 block")
(use-package use-package-ensure-system-package
  :after (system-packages)
  :demand t)
;; Block = @41482
(message "org-dotemacs: evaluating @41482 block")
(use-package anything
  :init
  (defun anything/goto-manual ()
    "Open up the anything PDF manual."
    (interactive)
    (find-file (straight-repos-dir "anything/doc/anything.pdf")))

  :config
  (require 'anything-config))

;; Block = @41813
(message "org-dotemacs: evaluating @41813 block")
(use-package prodigy)
;; Block = @42075
(message "org-dotemacs: evaluating @42075 block")
;; Modifer Keys

;; Mouse-2

;; Block = @42320
(message "org-dotemacs: evaluating @42320 block")
(setq mac-command-modifier 'super
      mac-option-modifier  'meta
      ns-control-modifier  'control
      ns-function-modifier 'hyper)

(when (eq system-type 'darwin)
  (global-set-key (kbd "s-=" ) 'text-scale-increase)
  (global-set-key (kbd "s--")  'text-scale-decrease)
  ;; Default is <XF86Back> .. C-x <right>
  (global-set-key (kbd "s-[")  'previous-buffer)
  (global-set-key (kbd "s-]")  'next-buffer)
  (global-set-key (kbd "s-}")  'ns-next-frame)
  (global-set-key (kbd "s-{")  'ns-prev-frame)
  (global-set-key (kbd "s-L")  'mark-sexp))

;; Block = @43274
(message "org-dotemacs: evaluating @43274 block")
;; From https://emacs.stackexchange.com/questions/20946/generate-mouse-2-event-from-macbook-trackpadTrackpage
(when (eq system-type 'darwin)
  (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>")))
;; Block = @43577
(message "org-dotemacs: evaluating @43577 block")
(defun macos-computer-name ()
  "Get the computer name for the current machine."
  (let* ((has-scutil
          (executable-find "scutil"))
	       (scutil-cmd
          (lambda ()
            (shell-command-to-string "scutil --get ComputerName"))))
    (if has-scutil
	      (replace-regexp-in-string "\n" "" (funcall scutil-cmd)) nil)))

(defvar computer-name nil)

(when (eq system-type 'darwin)
  (setq computer-name (macos-computer-name)))

;; Block = @44214
(message "org-dotemacs: evaluating @44214 block")
(use-package macos-dev-utils
  :if (eq system-type 'darwin)
  :straight
  (macos-dev-utils
   :host github
   :repo "jchaffin/macos-dev-utils"))

;; Block = @45331
(message "org-dotemacs: evaluating @45331 block")
(defvar dash-plugin-keywords nil
  "An `alist' of keywords representing the docsets which should
  searched in the query to Dash.app")

(defun macos-dash-at-point ()
  (interactive)
  (let* ((protocol "dash-plugin://")
         (keywords (if dash-plugin-keywords
                       (mapconcat 'identity dash-plugin-keywords ",")
                     ""))
         (search-string
          (if (use-region-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (substring-no-properties (or (thing-at-point 'symbol) "")))))

    (start-process "Dash" nil "open"
                   (concat
                    protocol
                    (unless (string-empty-p keywords)
                      (concat "keys=" keywords "&"))
                    "query="
                    (url-hexify-string search-string)))))
;; Block = @46305
(message "org-dotemacs: evaluating @46305 block")
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :commands (reveal-in-osx-finder))
;; Block = @46904
(message "org-dotemacs: evaluating @46904 block")
(use-package osx-dictionary
  :if (eq system-type 'darwin)
  :defines (osx-dictionary-open-dictionary-app-at-point)
  :commands (osx-dictionbary-search-word-at-point
             osx-dictionary-search-input)
  :init
  (progn
    (defun osx-dictionary-open-dictionary-app-at-point ()
      "Open `word' at point in Dictionary.app."
      (interactive)
      (shell-command (format "open dict://%s" (thing-at-point 'word))))))

;; Block = @47400
(message "org-dotemacs: evaluating @47400 block")
(use-package osx-trash
  :if (and (eq system-type 'darwin) (not (boundp 'mac-system-move-file-to-trash-use-finder)))
  :init
  (progn
    (osx-trash-setup))
  :config
  (progn
    (setq delete-by-moving-to-trash t)))
;; Block = @47711
(message "org-dotemacs: evaluating @47711 block")
(use-package pbcopy
  :if (and (eq system-type 'darwin) (not (display-graphic-p)))
  :init (turn-on-pbcopy))
;; Block = @47923
(message "org-dotemacs: evaluating @47923 block")
(use-package simpleclip)
;; Block = @48034
(message "org-dotemacs: evaluating @48034 block")
(use-package counsel-osx-app
  :demand t
  :preface
  (defun wk-osx-app-icon ()
    `(,(concat
        (propertize " " 'display '(raise -0.20))
        "Apps")))
  :if (eq system-type 'darwin)
  :custom
  (counsel-osx-app-location '("/Applications" "/Applications/Setapp"))
  :after (ivy)
  :commands (counsel-osx-app))

;; Block = @48528
(message "org-dotemacs: evaluating @48528 block")
  (when (eq system-type 'darwin)
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(global-set-key (kbd "C-x C-m i") 'set-input-method)

;; Block = @49250
(message "org-dotemacs: evaluating @49250 block")
(save-place-mode 1)
;; Block = @49640
(message "org-dotemacs: evaluating @49640 block")
(let ((directory (emacs-etc-dir "custom"))
      (file (pcase system-type
              (`darwin "custom-macos.el")
              (`gnu/linux "custom-linux.el")
              (`windows "custom-windows.el"))))
  (setq custom-file (expand-file-name file directory))
  ;; Create custom file if it does not exist.
  (if-not (file-exists-p custom-file)
      (with-temp-buffer
        (find-file custom-file)
        (save-buffer)
        (kill-buffer)))
  (load custom-file))

(defun goto-custom ()
    (interactive)
    (find-file custom-file))

(define-key goto-map "C" #'goto-custom)
;; Block = @50338
(message "org-dotemacs: evaluating @50338 block")
(setq make-backup-files nil)
;; Block = @50414
(message "org-dotemacs: evaluating @50414 block")
(setq auto-save-default nil)
(setq auto-save-no-message t)
;; Block = @50580
(message "org-dotemacs: evaluating @50580 block")
(setq create-lockfiles nil)
;; Block = @50731
(message "org-dotemacs: evaluating @50731 block")
(use-package autorevert
  :straight nil
  :init
  (global-auto-revert-mode t))

;; Block = @50981
(message "org-dotemacs: evaluating @50981 block")
(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-c C-s-." . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-M->" . mc/mark-next-lines)
   ("C-c C->" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (when (eq system-type 'darwin)
    (global-set-key (kbd "s-d")  'mc/mark-next-like-this)
    (global-set-key (kbd "s-D")  'mc/mark-all-dwin)
    (global-set-key (kbd "M-s-d" 'mc/edit-beginnings-of-lines))))
;; Block = @51627
(message "org-dotemacs: evaluating @51627 block")

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)

;; Block = @52538
(message "org-dotemacs: evaluating @52538 block")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
;; Block = @52697
(message "org-dotemacs: evaluating @52697 block")
(setq-default sentence-end-double-space nil)

;; Block = @52861
(message "org-dotemacs: evaluating @52861 block")
(use-package fix-word
  :bind
  (("M-u" . fix-word-upcase)
   ("M-l" . fix-word-downcase)
   ("M-c" . fix-word-capitalize)))
;; Block = @53061
(message "org-dotemacs: evaluating @53061 block")
(delete-selection-mode 1)
;; Block = @53147
(message "org-dotemacs: evaluating @53147 block")
(use-package smart-hungry-delete
  :init
  (smart-hungry-delete-add-default-hooks)
  ;; :bind (("<backspace>" . smart-hungry-delete-backward-char)
	;; 	     ("C-d" . smart-hungry-delete-forward-char))
  ;; :bind* (:map counsel-find-file-map
;;                (("<backspace>" . delete-backward-char)
;;                 ("C-d" . delete-forward-char)))
  )


;; Block = @53580
(message "org-dotemacs: evaluating @53580 block")
(use-package ack
  :if (executable-find "ack")
  :straight t)
;; Block = @53703
(message "org-dotemacs: evaluating @53703 block")
(use-package ag)
;; Block = @53766
(message "org-dotemacs: evaluating @53766 block")
(use-package grep+)
;; Block = @53850
(message "org-dotemacs: evaluating @53850 block")
(use-package rg
  :ensure-system-package
  (rg . ripgrep))

;; Block = @53996
(message "org-dotemacs: evaluating @53996 block")

  (use-package projectile-ripgrep
      :after (projectile)
      ;; takes a cons in the form of `(binary . package-name)`
      :ensure-system-package (rg . ripgrep))

;; Block = @54217
(message "org-dotemacs: evaluating @54217 block")
(use-package codesearch)
;; Block = @54303
(message "org-dotemacs: evaluating @54303 block")
(use-package counsel-codesearch
    :requires codesearch)
;; Block = @54454
(message "org-dotemacs: evaluating @54454 block")
(use-package projectile-codesearch)
;; Block = @54563
(message "org-dotemacs: evaluating @54563 block")
(use-package whole-line-or-region)
;; Block = @54650
(message "org-dotemacs: evaluating @54650 block")
(use-package wrap-region
  :demand t
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))))
  :init
  (wrap-region-mode t))
;; Block = @54994
(message "org-dotemacs: evaluating @54994 block")
(use-package expand-region
  :bind
   (("s-'" .  er/expand-region)
   ("s-S-'" . er/contract-region)))
;; Block = @55163
(message "org-dotemacs: evaluating @55163 block")
(use-package visual-regexp)
;; Block = @55251
(message "org-dotemacs: evaluating @55251 block")
(use-package replace-from-region)
;; Block = @55369
(message "org-dotemacs: evaluating @55369 block")
  (use-package flyspell
    :init
    (with-eval-after-load 'org
      (add-hook 'org-mode-hook 'flyspell-mode))
    :config
    (progn
      (when (executable-find "hunspell")
        (setq-default ispell-program-name "hunspell")
        (setq-default ispell-dictionary "en_US")
        (setq ispell-really-hunspell t))))
;; Block = @55783
(message "org-dotemacs: evaluating @55783 block")
  (use-package flyspell-correct-ivy
    :after (:all flyspell ivy)
    :demand t
    :config
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))
;; Block = @56128
(message "org-dotemacs: evaluating @56128 block")
(use-package langtool
  :if (eq system-type 'darwin)
  :after (flyspell)
  :demand t
  :custom
  (langtool-language-tool-jar
   "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
  (langtool-mother-tongue "en")
  (langtool-disabled-rules '("DASH_RULE"))
  :init
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                 ;; suppress popup after type `C-g` .
                 (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup))
;; Block = @56931
(message "org-dotemacs: evaluating @56931 block")
(use-package academic-phrases)
;; Block = @57033
(message "org-dotemacs: evaluating @57033 block")
  (with-eval-after-load 'flycheck
    (flycheck-define-checker proselint
                             "A linter for prose."
                             :command ("proselint" source-inplace)
                             :error-patterns
                             ((warning line-start (file-name) ":" line ":" column ": "
                                       (id (one-or-more (not (any " "))))
                                       (message) line-end))
                             :modes (text-mode org-mode markdown-mode gfm-mode))
    (add-to-list 'flycheck-checkers 'proselint))

;; Block = @58053
(message "org-dotemacs: evaluating @58053 block")
(use-package dictionary
  :commands (dictionary-lookup-definition)
  :init
  (define-prefix-command 'dictionary-keymap))
;; Block = @58312
(message "org-dotemacs: evaluating @58312 block")
 (use-package typo)
;; Block = @58449
(message "org-dotemacs: evaluating @58449 block")
 (use-package writegood-mode)
;; Block = @58568
(message "org-dotemacs: evaluating @58568 block")
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-find-dir-includes-top-level t)
  (projectile-enable-caching t)
  :init
    (el-patch-feature projectile)

  (el-patch-defun projectile-run-compilation (cmd)
    "Run external or Elisp compilation command CMD."
    (if (functionp cmd)
        (funcall cmd)
      (compile cmd (el-patch-add t))))

  (defvar halidom/ignored-project-directories
    '("~/.emacs.d/straight"))

  (defun projectile-ignore-projects-in-directory (project-root)
    "Ignore directories in `halidom/ignored-project-directories'."
    (cl-flet ((ignored-dir-or-subdir-p
               (path)
               (f-descendant-of?
                (f-expand project-root)
                path)))
      (->> halidom/ignored-project-directories
         (seq-filter #'ignored-dir-or-subdir-p)
         seq-empty-p not)))

  (setq projectile-ignored-project-function
        #'projectile-ignore-projects-in-directory)

  :config
  (when (featurep 'which-key)
    (which-key-add-key-based-replacements "C-c p" " Projectile")

    (push '((nil . "projectile-\\(.+\\)") . (nil . "\\1"))
          which-key-replacement-alist))

  (setq projectile-globally-ignored-directories
       (append projectile-globally-ignored-directories
                '("gradle" "target" ".meghanada"
                  ".gradle" "build" "bin" "node_modules"
                  "ltximg" "CMakeFiles" ".cquery_cached_index"))))

;; Block = @60217
(message "org-dotemacs: evaluating @60217 block")
  (use-package projectile-codesearch
      :after (projectile)
      :bind (:map projectile-command-map
                  ("s c" . projectile-codesearch-search)))
;; Block = @60463
(message "org-dotemacs: evaluating @60463 block")

  (use-package projectile-ripgrep
      :after (projectile)
      ;; takes a cons in the form of `(binary . package-name)`
      :ensure-system-package (rg . ripgrep))

;; Block = @60721
(message "org-dotemacs: evaluating @60721 block")
(use-package treemacs-projectile
  :after (treemacs)
  :demand t)
;; Block = @60868
(message "org-dotemacs: evaluating @60868 block")
(use-package find-file-in-project
  :bind ("s-p" . ffip))
;; Block = @60988
(message "org-dotemacs: evaluating @60988 block")
(use-package dired
  :straight nil
  :custom
  ;; When split frames with two dired buffers,
  ;; use other buffer as the current directory.
  (dired-dwim-target t)
  :bind (:map dired-mode-map
              ;; When moving to parent directory by `^´, Dired by default
              ;; creates a new buffer for each movement up. This rebinds
              ;; `^´ to use the same buffer.
              ("^" . dired/reuse-buffer)
              ("z" . dired-do-open-with))
  :init
  ;; from [[https://jblevins.org/log/dired-open][Integrating OS X and Emacs Dired]]:
  (defun dired-do-open-with (&optional arg)
    "Open the marked (or next ARG) files."
    (interactive "P")
    (cl-flet ((dired-open-with (file)
                               (start-process "default app" nil "open" file)))
      (let ((file-list (dired-get-marked-files t arg nil nil t)))
        (mapcar #'dired-open-with file-list))))

  (defun dired/reuse-buffer ()
    "Reuse the existing dired buffer when moving to the
parent directory."
	  (interactive)
    (find-alternate-file ".."))
  :config
  (when (symbolp 'org-file-apps)
    (add-to-list 'org-file-apps '(directory . emacs))))
;; Block = @62269
(message "org-dotemacs: evaluating @62269 block")
(use-package dired+
  :init
  (add-hook #'dired-mode-hook #'dired-hide-details-mode))
;; Block = @62506
(message "org-dotemacs: evaluating @62506 block")
 (use-package dired-sidebar
     :commands (dired-sidebar-toggle-sidebar)
     :custom
     (dired-sidebar-should-follow-file nil)
     (dired-sidebar-theme 'none)
     :bind
     ("C-c d" . dired-sidebar-toggle-sidebar)
     :hook
     (dired-sidebar-mode . dired-sidebar-refresh-buffer))
;; Block = @62845
(message "org-dotemacs: evaluating @62845 block")
(use-package pack
    :bind
  (:map dired-mode-map
        ("P" .  pack-dired-dwim)))
;; Block = @63149
(message "org-dotemacs: evaluating @63149 block")
(use-package neotree
  :after (projectile)
  :demand t
  :commands (neotree-project-dir)
  :bind
  (([f8] . neotree-project-dir))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-smart-open t)
  :config
  (progn
    ;; @source https://www.emacswiki.org/emacs/NeoTree
    (defun neotree-project-dir ()
      "Open NeoTree using the git root"
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                 (progn
                   (neotree-dir project-dir)
                   (neotree-find file-name)))
          (message "Project root not found."))))))
;; Block = @63936
(message "org-dotemacs: evaluating @63936 block")
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-python-executable (or (executable-find "python3") (executable-find "python")))
  (treemacs-collapse-dirs
   (if (executable-find "python3") 3 0))
  (treemacs-deferred-git-apply-delay   0.5)
  (treemacs-display-in-side-window     t)
  (treemacs-file-event-delay           5000)
  (treemacs-file-follow-delay          0.2)
  (treemacs-follow-after-init          t)
  (treemacs-follow-recenter-distance   0.1)
  (treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-indentation                2)
  (treemacs-indentation-string         " ")
  (treemacs-is-never-other-window      nil)
  (treemacs-max-git-entries            5000)
  (treemacs-no-png-images              nil)
  (treemacs-project-follow-cleanup     nil)
  (treemacs-persist-file
   (no-littering-expand-var-file-name "treemacs-persist"))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow  nil)
  (treemacs-show-cursor                nil)
  (treemacs-show-hidden-files          t)
  (treemacs-silent-filewatch           nil)
  (treemacs-silent-refresh             nil)
  (treemacs-sorting                    'alphabetic-desc)
  (treemacs-space-between-root-nodes   t)
  (treemacs-tag-follow-cleanup         t)
  (treemacs-tag-follow-delay           1.5)
  (treemacs-width                      35)
  :config

  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
;; Block = @66255
(message "org-dotemacs: evaluating @66255 block")
(use-package sr-speedbar)
;; Block = @66369
(message "org-dotemacs: evaluating @66369 block")
(use-package projectile-speedbar
  :after (:all speedbar projectile)
  :bind ("M-<f2>" . projectile-speedbar-open-current-buffer-in-tree))
;; Block = @66648
(message "org-dotemacs: evaluating @66648 block")
(use-package openwith
  :straight t)
;; Block = @66747
(message "org-dotemacs: evaluating @66747 block")
(use-package codesearch)
;; Block = @66869
(message "org-dotemacs: evaluating @66869 block")
  (use-package avy
    :bind
    ("C-:" . avy-goto-char))
;; Block = @66990
(message "org-dotemacs: evaluating @66990 block")
(use-package ack
  :if (executable-find "ack")
  :straight t)
;; Block = @67113
(message "org-dotemacs: evaluating @67113 block")
(use-package ag)
;; Block = @67176
(message "org-dotemacs: evaluating @67176 block")
(use-package grep+)

;; Block = @67256
(message "org-dotemacs: evaluating @67256 block")
(use-package rg
  :ensure-system-package
  (rg . ripgrep))

;; Block = @67371
(message "org-dotemacs: evaluating @67371 block")
(use-package visual-regexp)
;; Block = @67555
(message "org-dotemacs: evaluating @67555 block")
(use-package undo-tree
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :init
  (global-undo-tree-mode)
  :config
  (when (eq system-type 'darwin)
    (global-set-key (kbd "s-z") 'undo-tree-undo)
    (global-set-key (kbd "s-Z") 'undo-tree-redo)))
;; Block = @68032
(message "org-dotemacs: evaluating @68032 block")
(use-package ansi-color
  :custom
  (ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
  :init
  (defun colorize-compilation-buffer ()
    "Escape ANSI color sequence in the compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun display-ansi-colors ()
    "Display ANSI color sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; :config
  ;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  ;; (add-hook 'comint-output-filter-functions 'ansi-color-process-output)
  )
;; Block = @68761
(message "org-dotemacs: evaluating @68761 block")

 (use-package xterm-color
     :init
     ;; Comint and Shell
     (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
     (setq comint-output-filter-functions
           (remove 'ansi-color-process-output comint-output-filter-functions))

     (defun esh/xterm-color ()
       "Initialize xterm coloring for eshell."
    (with-eval-after-load 'eshell
      (setq-local xterm-color-preserve-properties t)
      (make-local-variable 'eshell-preoutput-filter-functions)
      (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
      (setq-local eshell-output-filter-functions
                  (remove 'eshell-handle-ansi-color
                          eshell-output-filter-functions))))

     :hook
     (eshell-mode . esh/xterm-color))

;; Block = @69612
(message "org-dotemacs: evaluating @69612 block")
(use-package eshell
  :custom
  (pcomplete-cycle-completions nil)
  (eshell-cmpl-cycle-completions nil)
  (eshell-buffer-maximum-lines 20000)
  (eshell-history-size 350)
  (eshell-hist-ignoredups t)
  (eshell-buffer-shorthand t)
  (eshell-highlight-prompt t)
  (eshell-plain-echo-behavior t)

  :init
  (defvar company-default-idle-delay nil)


  (defun esh/protect-prompt ()
    "Protect Eshell prompt like the prompt in `comint-mode'."
      (let ((inhibit-field-text-motion t))
        (add-text-properties
         (point-at-bol)
         (point)
         '(rear-nonsticky t
           inhibit-line-move-field-capture t
           field output
           read-only t
           front-sticky (field inhibit-line-move-field-capture)))))

  (defun esh/toggle-shell-completion-based-on-path ()
    "Deactivates automatic completion on remote paths. "
    (when (featurep 'company)
      (unless (numberp company-default-idle-delay)
        (setq company-default-idle-delay company-idle-delay))

      (if (file-remote-p default-directory)
          (setq-local company-idle-delay nil)
        (setq-local company-idle-delay company-default-idle-delay))))

  (defun esh/company ()
    "Change the company frontend to be compatible with short eshell
windows. Additionally, modify company backends in the local buffer."
    (when (featurep 'company)
      (setq-local company-frontends '(company-preview-frontend))
      (set (make-local-variable 'company-backends)
           (append '(company-capf) company-backends))))


  (defun esh/disable-semantic ()
    (when (featurep 'semantic)
      (semantic-mode -1)))

  :hook
  (eshell-mode . esh/company)
  (eshell-mode . esh/disable-semantic)
  (eshell-directory-change . esh/toggle-shell-completion-based-on-path))
;; Block = @71542
(message "org-dotemacs: evaluating @71542 block")
(use-package pretty-eshell
  :straight nil
  :load-path "etc/local/pretty-eshell"
  :init
  (progn
    (require 'pretty-eshell)
      ;; More prompt styling
    (setq pretty-eshell-header "\n︳")
    (setq eshell-banner-message (s-concat (s-repeat 20 "---") "\n\n"))
    (setq pretty-eshell-prompt-string " ")
    (setq eshell-prompt-regexp " ")
    ;; Directory
    (pretty-eshell-section
     esh-dir
     "\xf07c"  ; 
     (abbreviate-file-name (eshell/pwd))
     '(:foreground "gold" :weight bold :underline t))

    ;; Git Branch
    (pretty-eshell-section
     esh-git
     "\xe907"  ; 
     (magit-get-current-branch)
     '(:foreground "pink"))

    ;; Python Virtual Environment
    (pretty-eshell-section
     esh-python
     "\xe928"  ; 
     pyvenv-virtual-env-name)

    ;; Time
    (pretty-eshell-section
     esh-clock
     "\xf017"  ; 
     (format-time-string "%H:%M" (current-time))
     '(:foreground "forest green"))

    ;; Prompt Number
    (pretty-eshell-section
     esh-num
     "\xf0c9"  ; 
     (number-to-string pretty-eshell-prompt-num)
     '(:foreground "brown"))

    (setq pretty-eshell-funcs
          (list esh-dir esh-git esh-python esh-clock esh-num))))
;; Block = @72811
(message "org-dotemacs: evaluating @72811 block")
(use-package eshell-bookmark
  :hook
  (eshell-mode . eshell-bookmark-setup))
;; Block = @72952
(message "org-dotemacs: evaluating @72952 block")
(use-package eshell-z)
;; Block = @73043
(message "org-dotemacs: evaluating @73043 block")
(use-package shell
  :init

  (defun string-trim-final-newline (string)
    (let ((len (length string)))
      (cond
       ((and (> len 0) (eql (aref string (- len 1)) ?\n))
        (substring string 0 (- len 1)))
       (t string))))


  (defun shell-command-to-string-trim-final-newline
      (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (string-trim-final-newline res)))

  (advice-add 'shell-command-to-string
              :around #'shell-command-to-string-trim-final-newline)


  (defun inferior-shell (&optional ARG)
    "Wrapper to open shell in current window"
    (interactive)
    (switch-to-buffer "*shell*")
    (shell "*shell*"))

  (defun shell/comint-input-sender-hook ()
    "Check certain shell commands.
  Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (let ((inhibit-read-only  t))
                (erase-buffer)))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string
                             "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string
                             "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))

  (defun shell/disable-hl-line-mode ()
    "Locally disable `global-hl-line-mode'."
    (setq-local global-hl-line-mode nil))

  :hook
  (shell-mode . shell/comint-input-sender-hook)
  (shell-mode . shell/disable-hl-line-mode))
;; Block = @74984
(message "org-dotemacs: evaluating @74984 block")
(use-package term
  :custom
  (ansi-term-color-vector
   [term
    term-color-black
    term-color-red
    term-color-green
    term-color-yellow
    term-color-blue
    term-color-magenta
    term-color-cyan
    term-color-white])
  :init
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))

  (defun ansi-term-handle-close ()
    "Close current term buffer when `exit' from term buffer."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)"
                             change)
           (kill-buffer (process-buffer proc))
           (when (> (count-windows) 1)
             (delete-window)))))))
  :hook
  (term-mode . ansi-term-handle-close))
;; Block = @75926
(message "org-dotemacs: evaluating @75926 block")
(use-package multi-term
  :after (term)
  :bind
  (:map term-mode-map
  ( "C-c C-j" . term-line-mode))
  :init
  (defun multiterm (&optional ARG)
    "Wrapper to be able to call multi-term from shell-pop"
     (interactive)
     (multi-term))

  :config
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))
;; Block = @76309
(message "org-dotemacs: evaluating @76309 block")
(use-package shell-pop
  :custom
  (shell-pop-window-position 'bottom)
  (shell-pop-window-size 30)
  (shell-pop-term-shell shell-file-name)
  (shell-pop-full-span t)
  :init
  (defun resize-shell-pop-to-desired-width ()
    (when (and (string= (buffer-name) shell-pop-last-shell-buffer-name)
             (memq shell-pop-window-position '(left right)))
      (enlarge-window-horizontally
       (- (/ (* (frame-width) shell-default-width) 100)
          (window-width)))))

  (defmacro make-shell-pop-command (func &optional shell)
    "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
    (let* ((name (symbol-name func)))
      `(defun ,(intern (concat "shell-pop-" name)) (index)
         ,(format
           (concat "Toggle a popup window with `%S'.\n"
                   "Multiple shells can be opened with a numerical prefix "
                   "argument. Using the universal prefix argument will "
                   "open the shell in the current buffer instead of a "
                   "popup buffer.") func)
         (interactive "P")
         (require 'shell-pop)
         (if (equal '(4) index)
             ;; no popup
             (,func ,shell)
           (shell-pop--set-shell-type
            'shell-pop-shell-type
            (backquote (,name
                        ,(concat "*" name "*")
                        (lambda nil (,func ,shell)))))
           (shell-pop index)
           (resize-shell-pop-to-desired-width)))))

  (make-shell-pop-command eshell)
  (make-shell-pop-command term shell-pop-term-shell)
  (make-shell-pop-command ansi-term shell-pop-term-shell)
  (make-shell-pop-command inferior-shell)
  (make-shell-pop-command multiterm))
;; Block = @78139
(message "org-dotemacs: evaluating @78139 block")
(use-package with-editor
  :hook
  ((shell-mode term-exec eshell-mode) . with-editor-export-editor))
;; Block = @78302
(message "org-dotemacs: evaluating @78302 block")
(use-package ssh
  :init
  (defun ssh/enable-path-completion ()
    "Enable directory tracking and path autocompletion
over ssh."
    (setq ssh-directory-tracking-mode 'ftp)
    (shell-dirtrack-mode t)
    (setq dirtrackp nil))

  :hook
  (ssh-mode . ssh/enable-path-completion))
;; Block = @78635
(message "org-dotemacs: evaluating @78635 block")
(use-package ssh-tunnels)
;; Block = @78713
(message "org-dotemacs: evaluating @78713 block")
(use-package ssh-deploy)

;; Block = @78801
(message "org-dotemacs: evaluating @78801 block")
(use-package scp
  :preface
  (setq enable-local-variables :all enable-local-eval t)
  :init
  (cl-defun scp/setup (&optional host user password remote-path port)
    "Set the connection information for the current project using
directory local variables."
    (interactive (list
                  (read-string "Host: "
                               tramp-default-host)
                  (read-string "User: "
                               (or tramp-default-user
                                  (getenv "USER")))
                  (read-string "Password: ")
                  (read-string "Remote Path: "
                               (concat
                                "~/"
                                (basename default-directory)))
                  (read-string "Port: " "22")))
    (let ((vars
           (mapcar* #'cons
                    '(host user password remote-path port)
                    (list host user password remote-path port))))
      (dolist (var vars)
        (add-dir-local-variable nil (car var) (cdr var))))))
;; Block = @79917
(message "org-dotemacs: evaluating @79917 block")
(use-package tramp
  :custom
  ;; use ssh by default
  (tramp-default-method "ssh")
  (tramp-default-user user-login-name))
;; Block = @80182
(message "org-dotemacs: evaluating @80182 block")
(use-package docker-tramp)
;; Block = @80266
(message "org-dotemacs: evaluating @80266 block")
(use-package kubernetes-tramp)
;; Block = @80436
(message "org-dotemacs: evaluating @80436 block")
(use-package help+
    :demand t)
;; Block = @80522
(message "org-dotemacs: evaluating @80522 block")
(use-package help-mode+
    :demand t)
;; Block = @80614
(message "org-dotemacs: evaluating @80614 block")
(use-package help-macro+
    :demand t)
;; Block = @80705
(message "org-dotemacs: evaluating @80705 block")
(use-package help-fns+
    :demand t)
;; Block = @80812
(message "org-dotemacs: evaluating @80812 block")
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h C" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; Block = @81084
(message "org-dotemacs: evaluating @81084 block")
(use-package help-find-org-mode)
;; Block = @81167
(message "org-dotemacs: evaluating @81167 block")
(use-package elisp-refs)
;; Block = @81304
(message "org-dotemacs: evaluating @81304 block")
(use-package info+)
;; Block = @81533
(message "org-dotemacs: evaluating @81533 block")
(use-package frame+)
;; Block = @81602
(message "org-dotemacs: evaluating @81602 block")
(use-package frame-fns)
;; Block = @81743
(message "org-dotemacs: evaluating @81743 block")
(use-package frame-cmds)
;; Block = @81931
(message "org-dotemacs: evaluating @81931 block")
(use-package autofit-frame)
;; Block = @82009
(message "org-dotemacs: evaluating @82009 block")
(use-package fit-frame)
;; Block = @82121
(message "org-dotemacs: evaluating @82121 block")
(use-package ivy-posframe)
;; Block = @82270
(message "org-dotemacs: evaluating @82270 block")
(use-package faces+)
;; Block = @82345
(message "org-dotemacs: evaluating @82345 block")
(use-package face-fns)
;; Block = @82419
(message "org-dotemacs: evaluating @82419 block")
(use-package face-remap+)
;; Block = @82498
(message "org-dotemacs: evaluating @82498 block")
(use-package face-explorer)
;; Block = @82613
(message "org-dotemacs: evaluating @82613 block")
(use-package font-lock+
  :init
  (require 'font-lock+))

;; Block = @82846
(message "org-dotemacs: evaluating @82846 block")
(defcustom halidom-italicize-keyword-modes '(emacs-lisp-mode js2-mode)
  "Major modes for which an italicized font lock keyword
face shall be used."
  :type '(symbol))

(defun halidom/italicize-keyword-fn ()
(face-remap-add-relative 'font-lock-keyword-face
                         '(:slant italic
                           :family "Operator Mono")))

(defun halidom/italicize-keyword-faces ()
  (cl-flet ((mode->hook (mode)
              (intern (concat (symbol-name mode) "-hook"))))
    (cl-loop
       for mode in halidom-italicize-keyword-modes
       for hooksym = (mode->hook mode)
       do
         (add-hook hooksym 'halidom/italicize-keyword-fn))))


(add-hook 'after-init-hook 'halidom/italicize-keyword-faces)

;; Block = @83656
(message "org-dotemacs: evaluating @83656 block")
(use-package per-buffer-theme)
;; Block = @83744
(message "org-dotemacs: evaluating @83744 block")
(use-package font-lock-studio)
;; Block = @83913
(message "org-dotemacs: evaluating @83913 block")
(use-package button-lock)
;; Block = @84789
(message "org-dotemacs: evaluating @84789 block")
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)
;; Block = @85249
(message "org-dotemacs: evaluating @85249 block")
(use-package winner-mode
  :straight nil
  :init
  (when (fboundp 'winner-mode)
    (winner-mode 1)))
;; Block = @85420
(message "org-dotemacs: evaluating @85420 block")
  (use-package ace-window
      :bind
      ("M-o" . ace-window)
      :custom
      (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
      (aw-background nil))

;; Block = @85651
(message "org-dotemacs: evaluating @85651 block")
(use-package perspective)
;; Block = @85759
(message "org-dotemacs: evaluating @85759 block")
(use-package persp-projectile
  :after (:all projectile counsel-projectile perspective)
  :bind ((:map projectile-mode-map
               ("s-S" . projectile-persp-switch-project))))
;; Block = @86010
(message "org-dotemacs: evaluating @86010 block")
  (use-package popwin
    :defines popwin:keymap
    :after (perspective)
    :bind-keymap ("C-z" . popwin:keymap)
    :bind (:map popwin:keymap
                ("m" . popwin:messages))
    :init
    (require 'popwin)
    :config
    (popwin-mode 1))
;; Block = @86307
(message "org-dotemacs: evaluating @86307 block")
(use-package poporg
  :bind ("C-c \"" . 'poporg-dwim))
;; Block = @86414
(message "org-dotemacs: evaluating @86414 block")
(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale t))
;; Block = @86547
(message "org-dotemacs: evaluating @86547 block")
  (use-package window-purpose)

  (use-package ivy-purpose
      :custom (pop-up-frames nil)
      :after (window-purpose)
      :init
      (ivy-purpose-setup)
      :config
      ;; give help buffers the 'popup-frame purpose
      (add-to-list 'purpose-user-mode-purposes
                  '(help-mode . popup-frame))
      (add-to-list 'purpose-special-action-sequences
                   '(popup-frame
                     purpose-display-reuse-window-buffer
                     purpose-display-reuse-window-purpose
                     purpose-display-pop-up-frame)))
;; Block = @87651
(message "org-dotemacs: evaluating @87651 block")
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; Block = @88300
(message "org-dotemacs: evaluating @88300 block")
(use-package ivy
  :bind (("C-c C-r" . ivy-resume))
  :init
  (ivy-mode +1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)
  (ivy-sort-max-size 50000)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-ignore-order)))
  (ivy-use-selectable-prompt nil))
;; Block = @88791
(message "org-dotemacs: evaluating @88791 block")
(use-package ivy-hydra
  :after (hydra)
  :demand t)
;; Block = @88979
(message "org-dotemacs: evaluating @88979 block")
(use-package ivy-rtags
  :after (rtags)
  :init
  (setq rtags-display-result-backend 'ivy))

;; Block = @89137
(message "org-dotemacs: evaluating @89137 block")
(use-package ivy-xref
    :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
;; Block = @89293
(message "org-dotemacs: evaluating @89293 block")

  (use-package ivy-todo
    :custom
    (ivy-todo-file (agenda-dir "ivy-todo.org")))


;; Block = @89447
(message "org-dotemacs: evaluating @89447 block")
  (use-package ivy-rich
    :after (:all ivy counsel)
    :demand t
    :init
    (ivy-rich-mode 1)
    :config
    (defun ivy-rich-switch-buffer-icon (candidate)
      "Use `all-the-icons' icons in buffer list."
      (with-current-buffer
	  (get-buffer candidate)
	(let ((icon (all-the-icons-icon-for-mode major-mode)))
	  (if (symbolp icon)
	      (all-the-icons-icon-for-mode 'fundamental-mode)
	    icon))))

    (setq ivy-rich--display-transformers-list
     '(ivy-switch-buffer
       (:columns
	((ivy-rich-switch-buffer-icon :width 2)
	 (ivy-rich-candidate (:width 30))
	 (ivy-rich-switch-buffer-size (:width 7))
	 (ivy-rich-switch-buffer-indicators
	  (:width 4 :face error :align right))
	 (ivy-rich-switch-buffer-major-mode
	  (:width 12 :face warning))
	 (ivy-rich-switch-buffer-project (:width 15 :face success))
	 (ivy-rich-switch-buffer-path
	  (:width (lambda (x)
		    (ivy-rich-switch-buffer-shorten-path
		     x (ivy-rich-minibuffer-width 0.3))))))
	:predicate
	(lambda (cand) (get-buffer cand)))
       counsel-M-x
       (:columns
	((counsel-M-x-transformer (:width 40))
	 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns
	((counsel-describe-function-transformer (:width 40))
	 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
	((counsel-describe-variable-transformer (:width 40))
	 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
       counsel-recentf
       (:columns
	((ivy-rich-candidate (:width 0.8))
	 (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))))
;; Block = @91172
(message "org-dotemacs: evaluating @91172 block")
  (use-package ivy-pages
    :after (ivy))
;; Block = @91289
(message "org-dotemacs: evaluating @91289 block")
(use-package ivy-posframe)
;; Block = @91436
(message "org-dotemacs: evaluating @91436 block")
(use-package ivy-yasnippet)
;; Block = @91530
(message "org-dotemacs: evaluating @91530 block")
;; counsel mode

;; Counsel Projectile

;; Counsel Gtags

;; Counsel iTunes

;; Counsel spotify

;; Counsel Dash

;; Counsel Tramp

;; Counsel Code Search

;; Block = @91760
(message "org-dotemacs: evaluating @91760 block")
  (use-package counsel
    :bind (("<f2> u" . counsel-unicode-char)
           ("<f1> l" . counsel-find-library)
           ("C-c l" . counsel-load-library)
           ("C-c g" . counsel-git)
           ("C-c G" . counsel-git-grep)
           ("C-c k" . counsel-ag)
           ("C-x l" . counsel-locate)
           (:map minibuffer-local-map
                 ("C-r" . counsel-minibuffer-history))
           (:map org-mode-map
                 ("C-c C-j" . counsel-org-goto)
                 ("C-c M-t" . counsel-org-tag)
                 ("C-c f"   . counsel-org-file)))
    :init
    (counsel-mode +1)

    :config
    (setq-default counsel-git-grep-cmd counsel-git-grep-cmd-default)
    (setq counsel-mode-override-describe-bindings t)
    (when (featurep 'helpful)
      (setq counsel-describe-variable-function #'helpful-variable)
      (setq counsel-describe-function-function #'helpful-callable))
    :blackout t)

;; Block = @92902
(message "org-dotemacs: evaluating @92902 block")
(use-package counsel-projectile
    :after (:all projectile counsel)
    :demand t
    :init
    (counsel-projectile-mode t)
    ;; Use Dired
    (setcar counsel-projectile-switch-project-action 4))
;; Block = @93178
(message "org-dotemacs: evaluating @93178 block")
(use-package counsel-gtags
  :custom
	(counsel-gtags-ignore-case t)
  (counsel-gtags-auto-update t)
  :hook
  (c-mode-common . counsel-gtags-mode))
;; Block = @93404
(message "org-dotemacs: evaluating @93404 block")
(use-package counsel-itunes
  :if (executable-find "osascript")
  :straight
  (counsel-itunes
   :host github
   :repo "jchaffin/counsel-itunes")
  :demand t
  :preface
  (set-fontset-font
   "fontset-default" '(#xE030 .  #xE060)
   "Material Icons" nil 'prepend)
  :after (:all counsel ivy))

;; Block = @93778
(message "org-dotemacs: evaluating @93778 block")
(use-package counsel-spotify
  :straight t)
;; Block = @94335
(message "org-dotemacs: evaluating @94335 block")
  (use-package counsel-dash
    :after (:all counsel)
    :if (eq system-type 'darwin)
    :ensure-system-package
    ("/Applications/Dash.app" . "brew cask install dash"))

;; Block = @94595
(message "org-dotemacs: evaluating @94595 block")
(use-package counsel-codesearch
    :requires codesearch)
;; Block = @94729
(message "org-dotemacs: evaluating @94729 block")
(use-package counsel-tramp
  :after (counsel))
;; Block = @94840
(message "org-dotemacs: evaluating @94840 block")
(use-package swiper
    :custom
    (enable-recursive-minibuffers t)
    :bind
    ("\C-s" . swiper)
    :config
    (if (eq system-type 'darwin)
        (global-set-key (kbd "s-f") 'swiper)))
;; Block = @95099
(message "org-dotemacs: evaluating @95099 block")
;; Omnibox
(use-package omnibox
  :commands omnibox-M-x
  :bind (:map omnibox-mode-map
              ("M-x" . omnibox-M-x)))
;; Block = @95529
(message "org-dotemacs: evaluating @95529 block")
(use-package prescient
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))
;; Block = @95842
(message "org-dotemacs: evaluating @95842 block")
(use-package company-prescient
  :after (:all prescient company)
  :demand t
  :config
  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))
;; Block = @96173
(message "org-dotemacs: evaluating @96173 block")
(use-package ivy-prescient
  :init
  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

;; Block = @97008
(message "org-dotemacs: evaluating @97008 block")
(use-package company
  :commands global-company-mode
  :custom
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-idle-delay 0.2)
  :config
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :hook
  (after-init . global-company-mode))
;; Block = @97454
(message "org-dotemacs: evaluating @97454 block")
(use-package company-posframe
  :after (:all company posframe)
  :demand t
  :init
  (company-posframe-mode 1))

;; Block = @97652
(message "org-dotemacs: evaluating @97652 block")
(use-package company-dict
  :after (company)
  :demand t
  :init
  (add-to-list 'company-backends 'company-dict)
  :config
  (setq company-dict-enable-fuzzy t
        company-dict-enable-yasnippet t))
;; Block = @98129
(message "org-dotemacs: evaluating @98129 block")
(use-package company-quickhelp
  :after (company)
  :commands (company-quickhelp-manual-begin)
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1))
;; Block = @98423
(message "org-dotemacs: evaluating @98423 block")
(use-package company-box
    :custom (company-box-enable-icon nil))
;; Block = @98557
(message "org-dotemacs: evaluating @98557 block")
(use-package autoinsert
  :init
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.el$" [ "default-elisp.el" autoinsert-yas-expand ])
  :hook
  (find-file . auto-insert))

;; Block = @99075
(message "org-dotemacs: evaluating @99075 block")
(use-package yasnippet
    :bind
    (:map goto-map
          ("s" . goto-snippet-dir))

    :preface
    (defvar snippet-directory (emacs-etc-dir "yasnippet" "snippets")
      "Directory for yasnippets.")
    (defun goto-snippet-dir ()
      "Goto `snippet-directory'."
      (interactive)
      (let ((default-directory snippet-directory))
        (dired default-directory)))

    :init
    (yas-global-mode 1)
    :config

    (when (featurep 'which-key)
      (which-key-add-key-based-replacements
        "C-c &" " YASnippet")))
;; Block = @99665
(message "org-dotemacs: evaluating @99665 block")
(use-package ivy-yasnippet)
;; Block = @99752
(message "org-dotemacs: evaluating @99752 block")
(use-package auto-yasnippet)
;; Block = @99855
(message "org-dotemacs: evaluating @99855 block")
(use-package auto-yasnippet)
;; Block = @99932
(message "org-dotemacs: evaluating @99932 block")
(use-package header2)
;; Block = @100025
(message "org-dotemacs: evaluating @100025 block")
  (use-package skeletor
    :custom
    (skeletor-user-directory (emacs-etc-dir "skeletor/project-skeletons"))
    (skeletor-completing-read-function 'ivy-completing-read)
    (skeletor-python-bin-search-path
     '("/usr/local/bin" "/usr/bin"))
    (skeletor-project-directory (projects-dir))

    :init

    (defun skeletor-add-pyenv-pythons ()
      "Add python binaries managed by pyenv to
   `skeletor-python-bin-search-path'."
      (let* ((pyenv-dir (or (getenv "PYENV_ROOT")
                           (user-home ".pyenv")))
             (pyenv-version-dir
              (and pyenv-dir (resolve-path pyenv-dir "versions")))
             (pyenv-versions
              (directory-files-no-wildcards pyenv-version-dir t))
             (python-bins
              (mapcar
               (lambda (d)
                 (resolve-path d "bin"))
               pyenv-versions)))
        (dolist (python-bin python-bins)
          (add-to-list 'skeletor-python-bin-search-path
                       python-bin))))

    (when (executable-find "pyenv")
      (skeletor-add-pyenv-pythons))

    :config
    (add-to-list 'skeletor-global-substitutions
                 (cons "__TIME__"
                       (lambda () (format-time-string "%c"))))

    (skeletor-define-template "cmake-unix-makefiles"
      :requires-executables
      '(("cmake". "https://cmake.org")
        ("make" . "https://www.gnu.org/software/make"))

      :substitutions
      (list
       (cons "__DESCRIPTION__"
             (lambda ()
               (read-string "Description: ")))
       (cons "__TARGET_NAME__"
             (lambda ()
               (read-string "Target: ")))
       (cons "__PROJECT-VARS__" ".dir-locals"))

      :after-creation
      (lambda (dir)
        (skeletor-async-shell-command "mkdir build")
        (skeletor-async-shell-command
         (concat "("
                 " cd build &&"
                 " cmake -G 'Unix Makefiles'"
                 " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .. "
                 ")"))
        (skeletor-async-shell-command
         "ln -s `pwd`/build/compile_commands.json .")
        (dired dir)
        (revert-buffer)))



    (defun skeletor/projectile-ignore ()
      (with-eval-after-load 'projectile
        (add-to-list
         'projectile-project-ignored-directories
         skeletor--directory)))

    :hook
    (project-mode . skeletor/projectile-ignore))

;; Block = @102509
(message "org-dotemacs: evaluating @102509 block")
(use-package code-library
    :custom
    (code-library-directory (emacs-etc-dir "codelibrary"))
    (code-library-sync-to-gist t))
;; Block = @102812
(message "org-dotemacs: evaluating @102812 block")
(use-package artist-mode
  :straight nil
  :bind ((:map artist-mode-map
               ("C-c C-a p" . artist-select-op-pen-line))))
;; Block = @103025
(message "org-dotemacs: evaluating @103025 block")

(when (and (eq system-type 'darwin)
           (executable-find "brew")
           (executable-find "ditaa"))
  (let ((ditaa-path (replace-regexp-in-string
                     "\n"  ""
                     (shell-command-to-string
                      "realpath $(brew --prefix ditaa)/libexec/*.jar"))))
    (setq org-ditaa-jar-path ditaa-path)))

;; Block = @103446
(message "org-dotemacs: evaluating @103446 block")
(use-package plantuml-mode
  :if (executable-find "plantuml")
  :defines (org-plantuml-jar-path plantuml-jar-path)
  :init
  (defun halidom/plantuml-resolve-jar-path ()
      (when (executable-find "plantuml")
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string
          "realpath $(brew --prefix plantuml)/libexec/plantuml.jar"))))
  (when (eq system-type 'darwin)
    (setq org-plantuml-jar-path (halidom/plantuml-resolve-jar-path))
    (setq plantuml-jar-path (halidom/plantuml-resolve-jar-path))))
;; Block = @104030
(message "org-dotemacs: evaluating @104030 block")
(use-package graphviz-dot-mode
  :if (executable-find "dot")
  :mode "\\.dot\\'"
  :defines graphiz-dot-program-set
  :bind ((:map graphviz-dot-mode-map
               ("M-s g" . graphviz-dot-program-set)))
  :config
  (defun graphviz-dot-program-set (&optional program)
    "Set the process for `graphviz-dot-dot-program' interactively."
    (interactive)
    (let ((dot-program (or program (completing-read "Process: " graphviz-dot-layout-programs))))
      (setq graphviz-dot-dot-program dot-program)))

  (defun halidom/graphviz-dot-mode-setup ()
    "Setup graphviz dot mode."
    (define-key graphviz-dot-mode-map "{" nil)
    (define-key graphviz-dot-mode-map "}" nil)
    (add-to-list 'org-babel-load-languages '(dot . t))
    (setq org-src-lang-modes
          (append '(("dot" . graphviz-dot))
                  (delete '("dot" . fundamental) org-src-lang-modes))))

  (with-eval-after-load 'graphviz-dot-mode
    (halidom/graphviz-dot-mode-setup)))

;; Block = @105189
(message "org-dotemacs: evaluating @105189 block")

(use-package thesaurus
  :config
  (progn
    ;; `thesaurus-bhl-api-key' is set in secrets
    (setq thesaurus-prompt-mechanism 'counsel-imenu
          url-proxy-services nil)))

;; Block = @105599
(message "org-dotemacs: evaluating @105599 block")
  (use-package image+
    :if (display-graphic-p)
    :after (image)
    :config
    (eval-after-load 'image+
      `(when (require 'hydra nil t)
         (defhydra imagex-sticky-binding (global-map "C-x C-l")
           "Manipulating image"
           ("+" imagex-sticky-zoom-in "zoom in")
           ("-" imagex-sticky-zoom-out "zoom out")
           ("M" imagex-sticky-maximize "maximize")
           ("O" imagex-sticky-restore-original "restore orginal")
           ("S" imagex-sticky-save-image "save file")
           ("r" imagex-sticky-rotate-right "rotate right")
           ("l" imagex-sticky-rotate-left "rotate left")))))
;; Block = @106474
(message "org-dotemacs: evaluating @106474 block")
(use-package auctex
  :bind (:map LaTeX-mode-map
              ("M-s l" . TeX-engine-set)))
;; Block = @106718
(message "org-dotemacs: evaluating @106718 block")
(use-package company-auctex
  :demand t
  :after (:all company tex)
  :init
  (company-auctex-init))
;; Block = @106971
(message "org-dotemacs: evaluating @106971 block")
(use-package auctex-latexmk
  :after (tex)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :init
  (auctex-latexmk-setup))
;; Block = @107162
(message "org-dotemacs: evaluating @107162 block")
(use-package tex
  :straight auctex
  :custom
  ;; (TeX-command-default
  ;;  (if (executable-find "latexmk") "LatexMk" "LaTeX"))
  (TeX-error-overview-open-after-TeX-run t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-syntactic-comment t)
  ;; nonstopmode
  (TeX-interactive-mode nil)
  ;; Don't insert line-break at inline math
  (LaTeX-fill-break-at-separators nil)
  (LaTeX-item-indent nil)
  :init
  ;; https://emacs.stackexchange.com/a/19475
  (defun latex/pdfview ()
    "Use `pdf-view-mode' to open PDF files.
This requires the pdf-tools package to be installed."
    (when (file-exists-p (or pdf-info-epdfinfo-program ""))
      (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view))
      (setf (alist-get 'output-pdf TeX-view-program-selection) '("PDF Tools"))
      (setq TeX-source-correlate-start-server t)
      (add-hook 'TeX-after-compilation-finished-functions
                #'TeX-revert-document-buffer)))

  (defun latex/setup ()
    ;; Use the shell escape flag with `TeX-command'.
    (setq TeX-command-extra-options "-shell-escape")
    (when (fboundp 'flyspell-mode)
      (flyspell-mode +1))
    (when (fboundp 'doc-view-mode)
      (add-hook 'doc-view-mode 'auto-revert-mode))
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode +1))
    (when (fboundp 'ggtags-mode)
      (ggtags-mode +1))
    (when (fboundp 'typo-mode)
      (typo-mode -1))
    (when (fboundp 'smartparens-mode)
      (smartparens-mode +1)))

  :hook
  (LaTeX-mode . latex/setup)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . TeX-PDF-mode)

)

;; Block = @108995
(message "org-dotemacs: evaluating @108995 block")
(use-package company-math
  :after (company)
  :demand t
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode))
;; Block = @109241
(message "org-dotemacs: evaluating @109241 block")
  (use-package math-symbols
    :init

    (defun latex/font (math-input before after)
     "Insert a LaTeX font.

   MATH-INPUT is the input method to toggle in math environments.

   The BEFORE and AFTER arguments are the strings to insert around
   the marked region, when active. If the thing at point is a word,
   then wrap the word. Otherwise the two elements are joined at point."
     (cond ((texmathp)
            (if current-input-method
                (set-input-method nil)
              (set-input-method math-input)))
           ((region-active-p)
            (save-excursion
              (goto-char (region-beginning))
              (insert before)
              (goto-char (region-end))
              (insert after)))
           ((thing-at-point 'word)
            (save-excursion
              (goto-char (beginning-of-thing 'word))
              (insert before)
              (goto-char (end-of-thing 'word))
              (insert after)))
           (t
            (save-excursion
              (insert (concat before after)))
            (goto-char (+ (point) (length before))))))

    (defun latex/prettify-symbols-extra ()
      (if osx-browse-mode
          (osx-browse-mode nil))

      (when (fboundp 'prettify-symbols-mode)
        (prettify-symbols-mode 1))

      (setq prettify-symbols-alist
            '(("\\complement" . "∁"))))

    (setq-default abbrev-mode t)
    (setq latex-mode-abbrev-table nil)

    (define-abbrev-table 'latex-mode-abbrev-table
        '(("uiff"  "⟷")
          ("uif"  "⟶")
          ("uand" "∧")
          ("ulambda" "𝜆")
          ("uor" "∨")
          ("uxor" "⨁")
          ("uexists" "∃")
          ("unexists" "∄")
          ("usubset" "⊂")
          ("unsubset" "⊄")))

    (defun latex/unbind-osx-browse ()
      (when (fboundp 'osx-browse-mode)
        (let ((map osx-browse-mode-map))
          (define-key map (kbd "s-b") nil)
          (define-key map (kbd "s-i") nil))))

    (defun latex/font-italic ()
      "Insert a literal italic typeset command, or toggle input method
  in math environments with unicode math enabled."
      (interactive)
      (latex/font "math-symbols-italic" "\\textit{" "}"))

    (defun latex/font-bold ()
      "Insert a literal bold typeset command, or toggle input method
  in math environments with unicode math enabled."
      (interactive)
      (latex/font "math-symbols-bold" "\\textbf{" "}")
      (let ((map LaTeX-mode-map))
        (define-key map (kbd "s-b") 'latex/font-bold)
        (define-key map (kbd "s-i") 'latex/font-italic)))

    :hook
    (LaTeX-mode . latex/unbind-osx-browse)
    (LaTeX-mode . latex/prettify-symbols-extra))

;; Block = @111941
(message "org-dotemacs: evaluating @111941 block")
(use-package latex-extra
  :custom
  (latex/no-fill-environments
   '("align" "align*" "forest" "forest*"
     "equation" "equation*" "exe"
     "tabular" "tikzpicture"))

    :bind (:map LaTeX-mode-map
                ("C-c C-a" . latex/compile-commands-until-done)
                ("C-c C-n" . latex/next-section)
                ("C-c C-u" . latex/up-section)
                ("C-c C-f" . latex/next-section-same-level)
                ("C-M-f"   . latex/forward-environment)
                ("C-M-b"   . latex/backward-environment)
                ("C-M-a"   . latex/beginning-of-environment)
                ("C-c C-p" . latex/previous-section)
                ("C-c C-b" . latex/previous-section-same-level)
                ("C-c C-q" . latex/clean-fill-indent-environment))

    :init
    (defun latex/extra ()
      (latex-extra-mode +1))

    :hook
    (LaTeX-mode . latex/extra))

;; Block = @113034
(message "org-dotemacs: evaluating @113034 block")
(use-package reftex
    :straight nil
    :init
    (defun reftex/setup ()
      (turn-on-reftex)
      (setq reftex-plug-into-AUCTeX
            '(nil nil t t t)
            reftex-use-fonts t
            reftex-default-bibliography
            `(,(org-dir "ref/references.bib"))))
    :hook
    (LaTeX-mode . reftex/setup))
;; Block = @113512
(message "org-dotemacs: evaluating @113512 block")
(use-package company-reftex
  :demand t
  :after (:all company reftex))
;; Block = @113720
(message "org-dotemacs: evaluating @113720 block")
  (use-package magic-latex-buffer
      :custom
      (magic-latex-enable-block-highlight t)
      (magic-latex-enable-suscript nil)
      (magic-latex-enable-pretty-symbols t)
      (magic-latex-enable-block-align nil)
      (magic-latex-enable-inline-image nil)
      :hook
      (LaTeX-mode . magic-latex-buffer))
;; Block = @114107
(message "org-dotemacs: evaluating @114107 block")
(use-package texinfo
  :defines texinfo-section-list
  :commands texinfo-mode
  :mode
  ("\\.texi\\'" . texinfo-mode))
;; Block = @114407
(message "org-dotemacs: evaluating @114407 block")
(use-package latex-preview-pane
    :after (:all pdf-tools tex)
    :init (latex-preview-pane-enable))
;; Block = @114629
(message "org-dotemacs: evaluating @114629 block")
(let ((ltximg (file-truename '"~/.tmp/ltximg/")))
  (when (file-directory-p ltximg)
    (setq org-preview-latex-image-directory ltximg)))
;; Block = @114851
(message "org-dotemacs: evaluating @114851 block")
(defun org-preview-clear-cache ()
  (interactive)
  (let ((preview-cache
         (f-join default-directory org-preview-latex-image-directory)))
    (if (f-directory? preview-cache)
        (f-delete preview-cache t)
      (message "%s" "Directory 'ltximg' does not exist."))))
;; Block = @115229
(message "org-dotemacs: evaluating @115229 block")
(if (image-type-available-p 'imagemagick)
    (setq org-preview-latex-default-process 'imagemagick)
  (setq org-preview-latex-default-process 'dvisvgm))
;; Block = @115464
(message "org-dotemacs: evaluating @115464 block")
(when (boundp 'org-format-latex-options)
  (plist-put org-format-latex-options :scale 1.2))
;; Block = @115641
(message "org-dotemacs: evaluating @115641 block")
(defun org-preview/process-keyword  (p)
  (interactive)
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (if (string= p (org-element-property :key k))
          (intern (org-element-property :value k))))
    nil t))

(defun org-preview/process-compiler ()
  (or (org-preview/process-keyword "LATEX_COMPILER")
     org-latex-compiler))

(defun org-preview/set-process ()
  (or (org-preview/process-keyword "PREVIEW")
     org-preview-latex-default-process))

(defun org-preview/process-class ()
  (or (org-preview/process-keyword "LATEX_CLASS")
     org-latex-default-class))

(defun org-preview/orgling-p ()
  (let ((latex-class (org-preview/process-class)))
    (string= "orgling" latex-class)))

(defun org-preview/uclacs-p ()
  (let ((latex-class (org-preview/process-class)))
    (string= "uclacs" latex-class)))

;; Block = @116563
(message "org-dotemacs: evaluating @116563 block")
(defcustom xelatex-preview-header org-format-latex-header
  "The preamble to use for previewing LaTeX fragments with XeLaTeX."
  :type 'string)

(defcustom xelatex-preview-process-alist
  '((dvipng
     :programs
     ("xelatex" "dvipng")
     :description "dvi > png"
     :message "You need to install the programs: xelatex and dvipng"
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.3 1.3)
     :latex-compiler
     ("xelatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs
     ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: xelatex and dvisvgm."
     :use-xcolor t
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust
     (1.7 . 1.5)
     :latex-compiler
     ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("xelatex" "convert")
     :description "pdf > png"
     :message "You need to install xelatex and imagemagick"
     :use-xcolor t
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 1.0)
     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Preview engines for xelatex."
  :type '(alist
          :tag "LaTeX to image backends"
          :value-type (plist)))

(defun org-preview-xelatex ()
  (interactive)
  (let ((preview-file
         (emacs-etc-dir "preview/orgling-preview.tex")))
    (set (make-local-variable 'org-preview-latex-process-alist)
         xelatex-preview-process-alist)

    (when (assoc 'latex org-babel-load-languages)
      (set (make-local-variable 'org-edit-latex-frag-master)
           preview-file))

    (set (make-local-variable 'org-format-latex-header)
         (if (org-preview/orgling-p)
             (read-file-contents preview-file)
           xelatex-preview-header)))
  )
;; Block = @118768
(message "org-dotemacs: evaluating @118768 block")
(defcustom lualatex-preview-header nil
  "The preamble to use for previewing LaTeX fragments with LuLaTeX."
  :type 'string)

(defcustom lualatex-preview-process-alist
  '((dvipng
     :programs
     ("lualatex" "dvipng")
     :description "dvi > png"
     :message "You need to install the programs: lualatex and dvipng"
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.3 1.3)
     :latex-compiler
     ("lualatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs
     ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: lualatex and dvisvgm."
     :use-xcolor t
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust
     (1.7 . 1.5)
     :latex-compiler
     ("lualatex -no-pdf -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("lualatex" "convert")
     :description "pdf > png"
     :message "You need to install lualatex and imagemagick"
     :use-xcolor t
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 1.0)
     :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Preview engines for lualatex."
  :type '(alist
          :tag "LaTeX to image backends"
		      :value-type (plist)))


(defun org-preview-lualatex ()
  (interactive)
  (let ((preview-file (emacs-etc-dir "preview/uclacs-preview.tex")))
    (set (make-local-variable 'org-preview-latex-process-alist)
         lualatex-preview-process-alist)

    (when (assoc 'latex org-babel-load-languages)
      (set (make-local-variable 'org-edit-latex-frag-master)
           preview-file))

    (set (make-local-variable 'org-format-latex-header)
         (if (org-preview/uclacs-p)
             (concat lualatex-preview-header
                     (read-file-contents preview-file))
           lualatex-preview-header))))
;; Block = @121119
(message "org-dotemacs: evaluating @121119 block")
(setq compilation-scroll-output 'first-error)
;; Block = @121246
(message "org-dotemacs: evaluating @121246 block")
(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1)
  :config
  (setq scroll-step 1
        scroll-margin 10
        next-line-add-newlines nil
        scroll-preserve-screen-position 1)

  (setq mouse-wheel-follow-mouse 't)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))))
;; Block = @121626
(message "org-dotemacs: evaluating @121626 block")
  (use-package sublimity
      :init
    (require 'sublimity-scroll))
;; Block = @121874
(message "org-dotemacs: evaluating @121874 block")
(defun org-preview-with-compiler ()
  (interactive)
  (let ((latex-compiler (org-preview/process-compiler)))
    (when-let (preview-process (org-preview/set-process))
      (set (make-local-variable 'org-preview-latex-default-process)
           preview-process))

    (when (or (string= latex-compiler "xelatex")
             (string= latex-compiler "xetex"))
      (org-preview-xelatex))

    (when (string= latex-compiler "lualatex")
      (org-preview-lualatex))))

(add-hook 'org-mode-hook 'org-preview-with-compiler)
;; Block = @122466
(message "org-dotemacs: evaluating @122466 block")
(use-package cdlatex
  :custom
  ;; Disable auto label insertion in expanded template.
  ;; Labels conflict when used in conjunction with `org-ref'
  (cdlatex-insert-auto-labels-in-env-templates t)
  :hook
  ;; with AucTeX LaTeX mode
  ;; (LaTeX-mode . turn-on-cdlatex)
  ;; with Emacs latex mode
  ;; (latex-mode . turn-on-cdlatex)
  (org-mode . org-cdlatex-mode))
;; Block = @122911
(message "org-dotemacs: evaluating @122911 block")
(use-package org-edit-latex)
;; Block = @122986
(message "org-dotemacs: evaluating @122986 block")
(use-package bibtex
  :straight nil
  :custom
  (bibtex-BibTeX-entry-alist
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
  :config
  (defun bibtex-create-entries-buffer ()
  "Create a buffer listing the available bibtex
entry types and required fields."
  (interactive)
  (let ((buf (get-buffer-create "*BibTeX Entries*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+TITLE: BibTeX specification\n")
      (insert "* Entries\n")
      (loop for (type doc required crossref optional) in bibtex-BibTeX-entry-alist
            do
            (insert (format "\n** %s (%s)\n" type doc))
            (insert "\n*** Required fields\n\n")
            (loop for field in required
	                do
	                (insert (format "- %s" (car field)))
	                (message "%s" field)
	                (if (>= (length field) 2)
		                  (insert (format " :: %s\n" (nth 1 field)))
	                  (insert "\n")))
            (insert "\n*** Optional if Crossref present but otherwise required fields\n\n")
            (loop for field in crossref
	                do
	                (insert (format "- %s" (car field)))
	                (if (>= (length field) 2)
		                  (insert (format " :: %s\n" (nth 1 field)))
	                  (insert "\n")))
            (insert "\n*** Optional fields\n\n")
            (loop for field in optional
	                do
	                (insert (format "- %s" (car field)))
	                (if (>= (length field) 2)
		                  (insert (format " :: %s\n" (nth 1 field)))
	                  (insert "\n")))))
    (switch-to-buffer-other-window buf))))

;; Block = @131940
(message "org-dotemacs: evaluating @131940 block")

(use-package org-ref
  :after ivy
  :demand t
  :bind*
  (:map org-mode-map
        :prefix-map org-ref-prefix-map
        :prefix-docstring "Org ref citation manager."
        :prefix "C-c r"
        ("b" . org-ref-insert-bibliography-link)
        ("c" . org-ref-insert-cite-link)
        ("f" . org-ref-list-of-figures)
        ("g" . org-ref-insert-glossary-link)
        ("l" . org-ref-insert-label-link)
        ("r" . org-ref-insert-ref-link)
        ("s" .  org-ref-insert-bibliographystyle-link))
  :custom
  (org-ref-completion-library 'org-ref-ivy-cite)
  (org-ref-default-bibliography (org-dir "ref/references.org"))
  (org-ref-bibliography-notes (org-dir "ref/notes.org"))
  (org-ref-notes-directory (org-dir "ref/notes/"))
  (org-ref-pdf-directory (dropbox-dir "pdfs/" :slash t))
  (org-ref-show-citation-on-enter t)
  (org-ref-show-broken-links t)
  (org-ref-bibtex-hydra-key-binding "\C-cj")

  :init
  ;; https://github.com/jkitchin/org-ref/issues/428
  (progn
    (require 'doi-utils)
    (require 'org-ref-url-utils)
    (require 'org-id)
    (require 'org-ref-latex)
    (require 'org-ref-bibtex)
    (require 'org-ref-pdf)
    (require 'org-ref-scopus)
    (require 'org-ref-isbn)
    (require 'org-ref-pubmed)
    (require 'org-ref-arxiv)
    (require 'org-ref-sci-id)
    (require 'org-ref-wos)
    (require 'org-ref-worldcat)
    (require 'x2bib))

  :config

  (which-key-add-major-mode-key-based-replacements 'org-mode
     "C-c r" "Org Ref"
     "C-c r c" "Cite"
     "C-c r b" "Bibliography"
     "C-c r g" "Glossary"
     "C-c r f" "List of figures"
     "C-c r l"  "Labels"
     "C-c r r" "References"
     "C-c r s" "Bibliography style")

  (defun org-ref-vref-export (keyword desc format)
    "An export function for vref links."
    (cond
     ((eq format 'html)
      (format "<a href=\"#%s\">%s</a>" keyword (or desc keyword)))
     ((eq format 'latex) (format "\\vref{%s}" keyword))))


  (org-ref-link-set-parameters "vref"
    :follow #'org-ref-ref-follow
    :export #'org-ref-vref-export
    :complete #'org-ref-complete-link
    :face 'org-ref-ref-face-fn
    :help-echo #'org-ref-ref-help-echo)

  (setq org-ref-ref-types '("ref" "eqref" "pageref" "nameref"
                            "autoref" "cref" "Cref" "vref"))



  (defun org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'org-ref-open-pdf-at-point)


  (defun goto-org-ref-manual (&optional path)
    (interactive)
    (unless path
      (setq path (emacs-dir "straight" "repos" "org-ref" "org-ref.org")))
    (find-file path)))
;; Block = @135273
(message "org-dotemacs: evaluating @135273 block")
(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("h"   . pdf-annot-add-highlight-markup-annotation)
              ("t"   . pdf-annot-add-text-annotation)
              ("D"   . pdf-annot-delete))
  :init
  (pdf-tools-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1))
;; Block = @135867
(message "org-dotemacs: evaluating @135867 block")
(use-package org-pdfview
  :after (:all pdf-tools)
  :demand t
  :config
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (add-to-list 'org-file-apps
                             '("\\.pdf\\'" .
                               (lambda (file link)
                                 (org-pdfview-open link))))))))
;; Block = @136282
(message "org-dotemacs: evaluating @136282 block")
(use-package navi)
;; Block = @136350
(message "org-dotemacs: evaluating @136350 block")
(use-package outshine
  :init (setq outshine-minor-mode-prefix "\M-#")
  :hook ((outline-minor-mode . outshine-mode)))
;; Block = @136517
(message "org-dotemacs: evaluating @136517 block")
(use-package outorg
  :after (:all outshine))
;; Block = @137148
(message "org-dotemacs: evaluating @137148 block")
  (defun foldout-zoom-org-subtree (&optional exposure)
    "Same as `foldout-zoom-subtree' with often nicer zoom in Org mode."
    (interactive "P")
    (cl-letf
        (((symbol-function #'outline-show-entry) (lambda () (org-show-entry))))
      (foldout-zoom-subtree exposure)))
;; Block = @137524
(message "org-dotemacs: evaluating @137524 block")
  (use-package org-outline-numbering
    :custom-face
    (org-outline-numbering-face
     ((t (:family "Sans" :weight book :inherit (default)))))
    :init
    (defun org/outline-numbering ()
      (org-outline-numbering-mode 1)))

;; Block = @137820
(message "org-dotemacs: evaluating @137820 block")
(add-hook 'before-save-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode)
                       (eq buffer-read-only nil))
              (org-list-repair))))
;; Block = @138443
(message "org-dotemacs: evaluating @138443 block")
(use-package org-radiobutton
  :init
  (when (fboundp 'global-org-radiobutton-mode)
    (global-org-radiobutton-mode)))
;; Block = @138666
(message "org-dotemacs: evaluating @138666 block")
(defun halidom/tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(org-add-link-type
 "tag" 'halidom/tag-link)
;; Block = @139001
(message "org-dotemacs: evaluating @139001 block")
(use-package org-elisp-help)
;; Block = @139261
(message "org-dotemacs: evaluating @139261 block")
(org-link-set-parameters
 "youtube"
 :follow (lambda (path)
	   (browse-url (format "https://youtu.be/%s" path)))
 :export (lambda (path desc backend)
	   (cond
	    ((eq 'md backend)
	     (format "[%s](%s)

<a href=\"https://www.youtube.com/watch?v=%s\">
<img src=\"http://img.youtube.com/vi/%s/0.jpg\"></a>"
		     (or desc (format "https://youtu.be/%s" path))
		     (format "https://youtu.be/%s" path)
		     path path))))
 :help-echo "A youtube video. Click to open in browser.")

;; Block = @139832
(message "org-dotemacs: evaluating @139832 block")
(use-package org-link-minor-mode
  :hook (prog-mode . org-link-minor-mode))
;; Block = @139988
(message "org-dotemacs: evaluating @139988 block")
;; From https://orgmode.org/worg/org-tutorials/org-latex-export.html
(org-add-link-type
 "latex" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span class=\"%s\">%s</span>" path desc))
    ((eq format 'latex)
     (format "\\%s{%s}" path desc)))))

;; See https://lists.gnu.org/archive/html/emacs-orgmode/2014-08/msg00982.html
(setq org-latex-link-with-unknown-path-format "\\textsc{%s}")
;; Block = @140466
(message "org-dotemacs: evaluating @140466 block")
(use-package org-devonthink
  :straight nil
  :load-path "etc/local/org-devonthink")

;; Block = @140720
(message "org-dotemacs: evaluating @140720 block")
(use-package org-bookmark-heading
  :init
  (require 'org-bookmark-heading))
;; Block = @140952
(message "org-dotemacs: evaluating @140952 block")
  (defun org-check-percent-escapes ()
    "*Check buffer for possibly problematic old link escapes."
    (interactive)
    (when (eq major-mode 'org-mode)
      (let ((old-escapes '("%20" "%5B" "%5D" "%E0" "%E2" "%E7" "%E8" "%E9"
                           "%EA" "%EE" "%F4" "%F9" "%FB" "%3B" "%3D" "%2B")))
        (unless (boundp 'warning-suppress-types)
          (setq warning-suppress-types nil))
        (widen)
        (show-all)
        (goto-char (point-min))
        (while (re-search-forward org-any-link-re nil t)
          (let ((end (match-end 0)))
            (goto-char (match-beginning 0))
            (while (re-search-forward "%[0-9a-zA-Z]\\{2\\}" end t)
              (let ((escape (match-string-no-properties 0)))
                (unless (member (upcase escape) old-escapes)
                  (warn "Found unknown percent escape sequence %s at buffer %s, position %d"
                        escape
                        (buffer-name)
                        (- (point) 3)))))
            (goto-char end))))))
;; Block = @142149
(message "org-dotemacs: evaluating @142149 block")

  (defun org-extract-link ()
    "Extract the link location at point and put it on the killring."
    (interactive)
    (when (org-in-regexp org-bracket-link-regexp 1)
      (kill-new (org-link-unescape (org-match-string-no-properties 1)))))


(defun browse-url-extract-org-link (orig-fun &rest args)
  "If `thing-at-point' is a org-link, then call `org-extract-link'
  and apply ORIG-FUN with the extracted url in the car of original ARGS."
  (when (and (eq major-mode 'org-mode)
               (string= (car (org-thing-at-point)) "link"))
    (setcar args (org-extract-link)))
  (apply orig-fun args))

(advice-add 'osx-browse-url :around #'browse-url-extract-org-link)
;; Block = @142880
(message "org-dotemacs: evaluating @142880 block")
(use-package org-id
  :straight org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  :init
  ;; Custom id utilities
  ;; See [[https://writequit.org/articles/emacs-org-mode-generate-ids.html][Emacs Org Mode Generate Ids]]
  (with-eval-after-load 'org-id
    (el-patch-feature org-id)
    (el-patch-defun org-id-new (&optional prefix)
      "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org:4nd91V40HI\"."
      (let* ((prefix (if (eq prefix 'none)
		                     ""
		                   (concat (or prefix org-id-prefix) (el-patch-swap ":" "-"))))
	           unique)
        (if (equal prefix (el-patch-swap ":" "-")) (setq prefix ""))
        (cond
          ((memq org-id-method '(uuidgen uuid))
           (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
           (unless (org-uuidgen-p unique)
	           (setq unique (org-id-uuid))))
          ((eq org-id-method 'org)
           (let* ((etime (org-reverse-string (org-id-time-to-b36)))
	                (postfix (if org-id-include-domain
			                         (progn
			                           (require 'message)
			                           (concat "@" (message-make-fqdn))))))
	           (setq unique (concat etime postfix))))
          (t (error "Invalid `org-id-method'")))
        (concat prefix unique))))

  (defun org-custom-id-get (&optional pom create prefix)
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
           (org-id-add-location id (buffer-file-name (buffer-base id))))))))

  (defun org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
    file which do not already have one. Only adds ids if the
    `auto-id' option is set to `t' in the file somewhere. ie,
    ,#+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries (lambda () (org-custom-id-get (point) 'create))))))

  (defun org-add-custom-ids-before-save ()
    "Automatically adds custom ids to headlines in file before save."
    (add-hook 'before-save-hook
              (lambda ()
                (when (and (eq major-mode 'org-mode)
                         (eq buffer-read-only nil))
                  (org-add-ids-to-headlines-in-file)))))

  (defun org-capture-add-custom-id ()
    "Add a CUSTOM_ID property to headlines created by `org-capture'."
    (org-custom-id-get (point) 'create))

  :hook
  (org-capture-prepare-finalize . org-capture-add-custom-id)
  (org-mode . org-add-ids-to-headlines-in-file))

;; Block = @146638
(message "org-dotemacs: evaluating @146638 block")
(use-package ob-http
  :after (ob)
  :demand t)
;; Block = @146945
(message "org-dotemacs: evaluating @146945 block")
(use-package ob-clojurescript
  :if (executable-find "lumo")
  :after (ob)
  :demand t)
;; Block = @147082
(message "org-dotemacs: evaluating @147082 block")
(use-package ob-async)
;; Block = @147154
(message "org-dotemacs: evaluating @147154 block")
(use-package org-babel-eval-in-repl
  :after (eval-in-repl)
  :bind
  (:map org-mode-map
        ("C-<return>" . ober-eval-in-repl)
        ("C-c C-c" . ober-eval-block-in-repl)))



;; Block = @147388
(message "org-dotemacs: evaluating @147388 block")
(use-package ob-browser
  :ensure-system-package (ob-browser . "yarn add phantomjs"))
;; Block = @147526
(message "org-dotemacs: evaluating @147526 block")
(use-package ob-diagrams)
;; Block = @147633
(message "org-dotemacs: evaluating @147633 block")
  (org-babel-do-load-languages
   'org-babel-load-languages
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
;; Block = @148032
(message "org-dotemacs: evaluating @148032 block")
(use-package lentic)
;; Block = @148106
(message "org-dotemacs: evaluating @148106 block")
(use-package org2elcomment)
;; Block = @148192
(message "org-dotemacs: evaluating @148192 block")
(use-package org-drill
  :straight org
  :init
  (require 'org-drill))
;; Block = @148334
(message "org-dotemacs: evaluating @148334 block")
  (use-package org-attach
    :straight org
    :custom
    (org-attach-auto-tag "attach"))

;; Block = @148482
(message "org-dotemacs: evaluating @148482 block")
(use-package org-capture
    :straight org
    :custom
    (org-default-notes-file
     (expand-file-name "notes.org" org-directory)))
;; Block = @148682
(message "org-dotemacs: evaluating @148682 block")
(use-package org-capture-pop-frame)
;; Block = @148779
(message "org-dotemacs: evaluating @148779 block")
(use-package org-category-capture)
;; Block = @148936
(message "org-dotemacs: evaluating @148936 block")
(use-package alfred-org-capture
    :straight (alfred-org-capture
               :type git
               :host github
               :repo "jjasghar/alfred-org-capture"
               :files ("el/alfred-org-capture.el")))

;; Block = @149205
(message "org-dotemacs: evaluating @149205 block")
  (use-package org-habit
    :straight org
    :init
    (require 'org-habit))
;; Block = @149551
(message "org-dotemacs: evaluating @149551 block")
  (use-package org-agenda
    :straight org
    :custom
    (org-tags-column 0)
    (diary-file (expand-file-name "diary.org" org-directory))
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-deadline-prewarning-if-scheduled t)
    (org-agenda-time-leading-zero t)
    ;; http://cachestocaches.com/2016/9/my-workflow-org-agenda/
    (org-refile-targets (quote ((nil  :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9))))

    :init

    (defun goto-agenda-dir ()
      (interactive)
      (dired (agenda-dir)))


    (define-key goto-map "a" #'goto-agenda-dir)

    (setq org-agenda-category-icon-alist
     `(("global"     ,(list "")  nil nil :ascent center)
       ("syntax"     ,(list "")  nil nil :ascent center)
       ("phonetics"  ,(list "ə")  nil nil :ascent center)
       ("algorithms" ,(list "𝛺") nil nil :ascent center)
       ("languages"  ,(list "𝜆") nil nil :ascent center)))

    (defvar-local org-use-level-faces nil)
    (defvar org-level-remap-face nil)

    (defun halidom/remap-org-level-faces ()
      "Use minimal foreground face in `org-agenda-files' buffers."

      (let ((foreground (face-foreground 'default nil 'default)))
        (unless org-use-level-faces
          (mapcar
           (lambda (face)
             (add-to-list 'org-level-remap-face
                          (face-remap-add-relative
                           face
                           :foreground foreground)))
           org-level-faces)
          (setq-local org-use-level-faces t))))

    (defun halidom/org-agenda-file-face ()
      (cond ((org-agenda-file-p) (halidom/remap-org-level-faces))
            (org-use-level-faces
             (mapcar
              (lambda (f)
                (setq face-remapping-alist
                      (delq f face-remapping-alist)))
              org-level-remap-face))))


    (defvar halidom/default-agenda-file
      (file-truename  "Dropbox/org/todos/TODOs.org")
      "The file path of the default agenda file.")

    (when (file-exists-p halidom/default-agenda-file)
      (add-to-list 'org-agenda-files halidom/default-agenda-file))

    :hook
    (org-mode . halidom/org-agenda-file-face))


;; Block = @151793
(message "org-dotemacs: evaluating @151793 block")
(use-package org-super-agenda)

;; Block = @151893
(message "org-dotemacs: evaluating @151893 block")
(use-package org-agenda-ng
  :straight (org-agenda-ng :host github
                           :repo "alphapapa/org-agenda-ng"
                           :files ("org-agenda-ng.el")))

;; Block = @152124
(message "org-dotemacs: evaluating @152124 block")
(use-package org-ql
    :straight
    (org-ql
      :type git
      :host github
      :repo "alphapapa/org-agenda-ng"
      :files ("org-ql.el")))
;; Block = @152323
(message "org-dotemacs: evaluating @152323 block")
(use-package org-ql-agenda
    :straight
    (org-ql-agenda
      :type git
      :host github
      :repo "alphapapa/org-agenda-ng"
      :files ("org-ql-agenda.el")))
;; Block = @152567
(message "org-dotemacs: evaluating @152567 block")
(use-package org-sidebar
  :straight
  (org-sidebar
   :host github
   :repo "alphapapa/org-sidebar")

  :init
  (require 'org-ql)
  (require 'dash-functional)
  (require 'org-sidebar)

  :config
  (cl-defmacro course-sidebar (name description &optional extra)
    `(progn
       (org-sidebar-defsidebar
         ,(intern (concat name "/sidebar"))
         :header ,description
         :sidebars (((and (not (done))
                        (todo)
                        (or (deadline <=)
                           (scheduled <=))
                        (not (tags "optional")))))
         :super-groups ',(append
                          '((:name " Overdue" :scheduled past :deadline past)
                            (:name " Today" :scheduled today :deadline today)
                            (:name " Homework" :tag "homework"))
                          (list extra))
         :files ,(expand-file-name
                  "TODOs.org"
                  (concat "~/Dropbox/courses/" name "/")))
       (defun ,(intern (concat name "-sidebar")) ()
         (interactive)
         (funcall ',(intern (concat name "/sidebar"))))))

  (cl-defun org-course-sidebar ()
    (interactive)
    (let ((todo-file
           (car (directory-files
                 (projectile-project-root) t "TODOs.org")))
          (sidebar-function
           (intern
            (concat
             (basename (projectile-project-root))
             "-sidebar"))))
      (when (and (member todo-file org-agenda-files)
               (functionp sidebar-function))
        (message "%s" sidebar-function)
        (funcall sidebar-function)))))
;; Block = @154374
(message "org-dotemacs: evaluating @154374 block")
  (use-package org-projectile
    :commands (org-projectile-files-to-agenda)
    :after (projectile)
    :bind (:map projectile-command-map
                ("n" . org-projectile-project-todo-completing-read))
    :custom
    (org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)")
            (sequence "|" "CANCELLED(c)")))
    (org-todo-keyword-faces
     '(("CANCELLED" . (:foreground "yellow"))))
    (org-projectile-per-project-filepath "TODOs.org")

    :init
    (defvar-local org-default-agenda-file
      (file-truename "~/Dropbox/org/todos/TODOS.org"))

    (defun org-projectile-files-to-agenda ()
      "Add projectile project files to agenda."
      (interactive)
      (cl-flet*
          ((project-agenda-filepath (p)
                                    (expand-file-name
                                     "TODOs.org"
                                     p))
           (todo-file-p (p)
                        (file-exists-p (project-agenda-filepath p)))
           (build-agenda (ps)
                         (setq org-agenda-files
                               (seq-uniq (append org-agenda-files ps)))))

          (->> projectile-known-projects
             (seq-filter #'todo-file-p)
             (mapcar #'project-agenda-filepath)
             build-agenda)))

    :hook
    ((org-agenda-mode dashboard-mode) . org-projectile-files-to-agenda)

    :config
    (org-projectile-per-project))

;; Block = @156012
(message "org-dotemacs: evaluating @156012 block")
;; (use-package org-gcal
;;     :init
;;   (require 'secrets)
;;   (require 'org-gcal)
;;   :custom
;;   (org-gcal-file-alist
;;    '(("jchaffin@g.ucla.edu" . "~/Dropbox/org/agenda/schedule.org")))
;;   :hook
;;   (org-agenda-mode . org-gcal-sync)
;;   (org-capture-after-finalize . org-gcalcc-sync))

;; Block = @156391
(message "org-dotemacs: evaluating @156391 block")
(use-package google-maps)
;; Block = @156489
(message "org-dotemacs: evaluating @156489 block")
(use-package calfw
  :custom
  (cfw:org-capture-template
   '("c" "calfw2org" entry (file "agenda/schedule.org")  "* %?n %(cfw:org-capture-day)"))
  (cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)
  (cfw:display-calendar-holidays nil)
  (cfw:fchar-junction ?╋)
  (cfw:fchar-vertical-line ?┃)
  (cfw:fchar-horizontal-line ?━)
  (cfw:fchar-left-junction ?┣)
  (cfw:fchar-right-junction ?┫)
  (cfw:fchar-top-junction ?┯)
  (cfw:fchar-top-left-corner ?┏)
  (cfw:fchar-top-right-corner ?┓)

  :hook
  (cfw:details-mode . org-link-minor-mode))

(use-package calfw-org
  :demand t
  :custom
  (cfw:org-capture-template
   '("c" "calfw2org" entry
     (file "agenda/schedule.org")
     "*  %?\n %(cfw:org-capture-day)"))
  :init
  (require 'calfw)
  (defun cfw:open-calendar ()
   (interactive)
   (let ((cp
          (cfw:create-calendar-component-buffer
           :view 'month
           :contents-sources
           (list
            (cfw:org-create-file-source
             "syntax" "~/Dropbox/courses/ling165b/TODOS.org" "#91E5DD")
            (cfw:org-create-file-source
             "phonetics" "~/Dropbox/courses/ling103/TODOs.org" "purple")
            (cfw:org-create-file-source
             "programming languages" "~/Dropbox/courses/cs131/TODOs.org" "tomato1")))))
     (switch-to-buffer (cfw:cp-get-buffer cp))))
)
;; Block = @157981
(message "org-dotemacs: evaluating @157981 block")
(defun org-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
          (min (string-to-number (match-string 2 s)))
          (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
          (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))
(defun org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
        ((>= secs 60) (format-seconds "%m:%.2s" secs))
        (t (format-seconds "%s" secs))))

(defmacro with-time (time-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
  (list
   (if time-output-p 'org-time-seconds-to-string 'identity)
   (cons 'progn
         (mapcar
          (lambda (expr)
            `,(cons (car expr)
                    (mapcar
                     (lambda (el)
                       (if (listp el)
                           (list 'with-time nil el)
                         (org-time-string-to-seconds el)))
                     (cdr expr))))
          `,@exprs))))

;; Block = @159701
(message "org-dotemacs: evaluating @159701 block")
(use-package org-pomodoro
  :bind (:map org-mode-map
              ("C-c M-RET p" . org-pomodoro))
  :config
  (progn
    (defalias #'org-pomodoro-path
      (apply-partially #'emacs-etc-dir "org/pomodoro"))

    (setq org-pomodoro-audio-player "/usr/bin/afplay"
          org-pomodoro-tick-sound
          (org-pomodoro-path "clock-ticking-2.wav")
          ;; Start Settings
          org-pomodoro-start-sound-p t ;; enable starting sound
          org-pomodoro-start-sound-args "--volume 0.08"
          org-pomodoro-start-sound
          (org-pomodoro-path "Victory.wav")
        ;; Finished Settings
          org-pomodoro-finished-sound-args "--volume 0.2"
          org-pomodoro-finished-sound
          (org-pomodoro-path "Waves.wav")
          ;; Short Break Settings
          org-pomodoro-short-break-length 5
          org-pomodoro-short-break-sound-args "--volume 0.2"
          org-pomodoro-short-break-sound org-pomodoro-finished-sound
          ;; Long Break Settings
          org-pomodoro-long-break-length 15
          org-pomodoro-long-break-sound-args "--volume 0.2"
          org-pomodoro-long-break-sound
          (org-pomodoro-path "Complete.wav"))))
;; Block = @160963
(message "org-dotemacs: evaluating @160963 block")
(use-package counsel-org-clock
  :straight (:host github
                   :repo "akirak/counsel-org-clock")
  :after (:all org-agenda ivy))
;; Block = @161321
(message "org-dotemacs: evaluating @161321 block")
(use-package org-mru-clock
  :after (:all org-agenda ivy)
  :demand t
  :bind (("C-c C-x i" . org-mru-clock-in)
         ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :init
  (progn
    (setq org-mru-clock-how-many 50
          org-mru-completing-read #'ivy-completing-read)))
;; Block = @161782
(message "org-dotemacs: evaluating @161782 block")
(use-package org-clock-convenience)
;; Block = @161893
(message "org-dotemacs: evaluating @161893 block")
(use-package org-brain
  :if (eq system-type 'darwin)
  :defines (org-brain-path)
  :init
  (setq org-brain-path (concat (file-name-as-directory org-directory) "brain"))
  :config
  (progn
    (setq org-id-track-globally t
          org-brain-visualize-default-choices 'all
          org-brain-title-max-length 12)
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)))
;; Block = @162593
(message "org-dotemacs: evaluating @162593 block")
(setq org-image-actual-width nil)
;; Block = @163283
(message "org-dotemacs: evaluating @163283 block")
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("‣" "•"))
  :init
  (require 'org-bullets)
  (defun org-bullets/enable ()
    (org-bullets-mode +1))

    (defun org-bullets/pretty ()
      (setq org-pretty-entities t)
      (setq org-pretty-entities-include-sub-superscripts nil)
      (setq org-hide-emphasis-markers t)
      (setq org-fontify-quote-and-verse-blocks t))

    :hook
    (org-mode . org-bullets/enable)
    (org-bullets . org-bullets/pretty))

;; Block = @163819
(message "org-dotemacs: evaluating @163819 block")
(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
;; Block = @164044
(message "org-dotemacs: evaluating @164044 block")
(use-package org-pretty-table
  :straight (org-pretty-table
             :host github
             :type git
             :repo "Fuco1/org-pretty-table"))
;; Block = @164776
(message "org-dotemacs: evaluating @164776 block")
(use-package toc-org
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))
;; Block = @164928
(message "org-dotemacs: evaluating @164928 block")
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
;; Block = @165251
(message "org-dotemacs: evaluating @165251 block")
(defun org-renumber-environment (orig-func &rest args)
  (let ((results '())
        (counter -1)
        (numberp))

    (setq results
          (loop for (begin .  env) in
               (org-element-map (org-element-parse-buffer) 'latex-environment
                 (lambda (env)
                   (cons
                    (org-element-property :begin env)
                    (org-element-property :value env))))
             collect
               (cond
                 ((and (string-match "\\\\begin{equation}" env)
                     (not (string-match "\\\\tag{" env)))
                  (incf counter)
                  (cons begin counter))
                 ((string-match "\\\\begin{align}" env)
                  (prog2
                      (incf counter)
                      (cons begin counter)
                    (with-temp-buffer
                      (insert env)
                      (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                      (incf counter (count-matches "\\\\$"))
                      ;; unless there are nonumbers.
                      (goto-char (point-min))
                      (decf counter (count-matches
                                     "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))

;; (advice-add 'org-create-formula-image
;;           :around #'org-renumber-environment)

;; Block = @167093
(message "org-dotemacs: evaluating @167093 block")
(setq org-highlight-latex-and-related '(latex))
;; Block = @167317
(message "org-dotemacs: evaluating @167317 block")
(defun modi/org-entity-get-name (char)
  "Return the entity name for CHAR. For example, return \"ast\" for *."
  (let ((ll (append org-entities-user
                    org-entities))
        e name utf8)
    (catch 'break
      (while ll
        (setq e (pop ll))
        (when (not (stringp e))
          (setq utf8 (nth 6 e))
          (when (string= char utf8)
            (setq name (car e))
            (throw 'break name)))))))

(defun modi/org-insert-org-entity-maybe (&rest args)
  "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
  ;; It would be fine to use just (this-command-keys) instead of
  ;; (substring (this-command-keys) -1) below in emacs 25+.
  ;; But if the user pressed "C-u *", then
  ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
  ;;  - in emacs 25.x, (this-command-keys) would return "*".
  ;; But in both versions, (substring (this-command-keys) -1) will return
  ;; "*", which is what we want.
  ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
  (let ((pressed-key (substring (this-command-keys) -1))
        entity-name)
    (when (and (listp args) (eq 4 (car args)))
      (setq entity-name (modi/org-entity-get-name pressed-key))
      (when entity-name
        (setq entity-name (concat "\\" entity-name "{}"))
        (insert entity-name)
        (message (concat "Inserted `org-entity' "
                         (propertize entity-name
                                     'face 'font-lock-function-name-face)
                         " for the symbol "
                         (propertize pressed-key
                                     'face 'font-lock-function-name-face)
                         "."))))
    entity-name))

;; Run `org-self-insert-command' only if `modi/org-insert-org-entity-maybe'
;; returns nil.
(advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)

;; Block = @169493
(message "org-dotemacs: evaluating @169493 block")
(use-package org-index)
;; Block = @169614
(message "org-dotemacs: evaluating @169614 block")
(use-package org-noter)
;; Block = @169729
(message "org-dotemacs: evaluating @169729 block")
  (use-package org-journal
    :custom
    (org-journal-enable-agenda-integration t)
    (org-journal-dir (org-dir "journal"))
    (org-journal-date-prefix "#+TITLE: ")
    (org-journal-date-format "%A, %B %d %Y")
    (org-journal-time-prefix "* ")
    (org-journal-time-format "")
    :config
    (with-eval-after-load 'org-capture
      (push '("j" "Journal" entry
              (file+olp+datetree "~/Dropbox/org/journal.org")
            "* %?\nEntered on %U\n %i\n %a")
            org-capture-templates)))

;; Block = @170535
(message "org-dotemacs: evaluating @170535 block")
(use-package ox-extra
  :straight org
  :demand t
  :config
  (ox-extras-activate
   '(ignore-headlines
     org-export-filter-parse-tree-functions)))
;; Block = @170755
(message "org-dotemacs: evaluating @170755 block")
(use-package ox-publish
  :straight org
  :after (ox)
  :demand t)
;; Block = @170883
(message "org-dotemacs: evaluating @170883 block")
(defun org-to-org-handle-includes ()
  "Copy the contents of the current buffer to OUTFILE,
recursively processing #+INCLUDEs."
  (let* ((s (buffer-string))
     (fname (buffer-file-name))
     (ofname (format "%s.I.org" (file-name-sans-extension fname))))
    (setq result
      (with-temp-buffer
        (insert s)
        (org-export-handle-include-files-recurse)
        (buffer-string)))
    (find-file ofname)
    (delete-region (point-min) (point-max))
    (insert result)
    (save-buffer)))
;; Block = @171492
(message "org-dotemacs: evaluating @171492 block")
(require 'ox-latex)
;; Prefer user labels

;; Subfigure

;; PDF Process Interface

;; Hyperref Template

;; Custom classes

;; Block = @171705
(message "org-dotemacs: evaluating @171705 block")
(setq org-latex-prefer-user-labels t)
;; Block = @171822
(message "org-dotemacs: evaluating @171822 block")
(use-package ox-latex-subfigure
  :straight
  (ox-latex-subfigure
   :type git
   :host github
   :repo "linktohack/ox-latex-subfigure")
  :config
  (require 'ox-latex-subfigure)
  (add-to-list 'org-latex-packages-alist (("" "subcaption" nil))))

;; Block = @172275
(message "org-dotemacs: evaluating @172275 block")
(eval-and-compile
  (defvar enable-default-minted nil))

(setq org-latex-listings 'minted)

(setq org-latex-packages-alist '(("" "booktabs" nil)))
(setq org-latex-tables-booktabs t)

(setq org-latex-minted-options
      '(("mathescape" "true")
        ("linenos" "true")
        ("breaklines" "true")
        ("numbersep" "5pt")
        ("frame" "lines")
        ("framesep" "2mm")))

(defun latex-toggle-default-minted ()
  (interactive)
  (if (not enable-default-minted)
      (progn
        (setq org-latex-packages-alist
              (append org-latex-packages-alist '(("newfloat" "minted"))))
        (setq enable-default-minted t)
        (message "%s" "Exporting with default minted."))
    (progn
      (setq enable-default-minted nil)
      (dolist (elt org-latex-packages-alist)
        (if (string= (cadr elt) "minted")
            (setq org-latex-packages-alist (remove elt org-latex-packages-alist))))
      (message "%s" "Disabled default minted."))))


;; Block = @173514
(message "org-dotemacs: evaluating @173514 block")
(setq latex-process-latex
  '("%latex -interaction nonstopmode -output-directory %o %f"
    "%latex -interaction nonstopmode -output-directory %o %f"
    "%latex -interaction nonstopmode -output-directory %o %f"))

(setq latex-process-pdflatex
  '("latexmk -pdflatex='pdflatex -interaction=nonstopmode -shell-escape' -synctex=1 -pdf -bibtex -f %f"))

(setq latex-process-xelatex
  '("latexmk -pdf -synctex=1 -shell-escape -xelatex -f %f"))

(setq latex-process-lualatex
  '("latexmk -pdf -synctex=1 -shell-escape -lualatex -f %f"))

(defcustom halidom-latex-pdf-engines
  '(("latex" . latex-process-latex)
    ("lualatex" . latex-process-lualatex)
    ("xelatex" . latex-process-xelatex)
    ("pdflatex" . latex-process-pdflatex))
  "A list of LaTeX commands available to run when
`org-latex-export-to-pdf' is invoked."
  :type '(cons string symbol))

(setq org-latex-pdf-process latex-process-xelatex)

(defvar org-latex-pdf-process-set-hook nil)

(defun org-latex-pdf-process-set (&optional process)
  (interactive)
  (let* ((process (or
                   process
                   (assoc (completing-read "Process: " halidom-latex-pdf-engines nil nil)
                          halidom-latex-pdf-engines)))
         (cmd-string (cdr process)))
    (setq org-latex-pdf-process (symbol-value cmd-string))
    (setq org-latex-compiler (car process))
    (run-hooks 'org-latex-pdf-process-set-hook)))


(bind-keys :map org-mode-map
            ("M-s l" . org-latex-pdf-process-set))

;; Block = @175192
(message "org-dotemacs: evaluating @175192 block")
;; TODO: Dynamically set to a sensible default if using 'article class
;; or a class in `org-latex-classes' that includes hyperref.
(setq org-latex-hyperref-template nil)
;; Block = @175438
(message "org-dotemacs: evaluating @175438 block")
;; Org Ling

;; UCLA CS

;; Humanities

;; Article No Default Packages

;; Unicode math

;; Block = @175740
(message "org-dotemacs: evaluating @175740 block")
(add-to-list 'org-latex-classes
	     '("article-standalone"
	       "\\documentclass{article}
          [NO-DEFAULT-PACKAGES]
          [PACKAGES]
          [EXTRA]" ;; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Block = @176258
(message "org-dotemacs: evaluating @176258 block")
(if (and (executable-find "kpsewhich")
         (shell-command-to-string "kpsewhich orgling.cls"))

    (add-to-list 'org-latex-classes
                 '("orgling"
                   "\\documentclass{orgling}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; Block = @176927
(message "org-dotemacs: evaluating @176927 block")
(add-to-list 'org-latex-classes
             '("uclacs"
               "\\documentclass{uclacs}
                [NO-DEFAULT-PACKAGES]
                [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Block = @177494
(message "org-dotemacs: evaluating @177494 block")
(add-to-list 'org-latex-classes
             '("humanities"
               "\\documentclass{humanities}
                [NO-DEFAULT-PACKAGES]
                [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Block = @178039
(message "org-dotemacs: evaluating @178039 block")
  (add-to-list 'org-latex-classes
    '("unicode-math"
      "\\documentclass{article}
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
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Block = @179007
(message "org-dotemacs: evaluating @179007 block")
(use-package ox-linguistics
    :straight (ox-linguistics
               :host github
               :repo "jchaffin/ox-linguistics"
               :files ("lisp/*.el"))
    :after (:all ox ox-latex)
    :demand t)
;; Block = @179267
(message "org-dotemacs: evaluating @179267 block")
(use-package gb4e)
;; Block = @179353
(message "org-dotemacs: evaluating @179353 block")
(use-package ox-bibtex
  :straight org
  :mode
  (("\\.org.bib\\'" . org-mode))
  :after (ox)
  :demand t
  :config
  (progn
    (require 'org-bibtex)
    (setq org-bibtex-file "references.org")))
;; Block = @179875
(message "org-dotemacs: evaluating @179875 block")
(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after (:all ox org-ref)
  :custom

  (org-pandoc-options '((standalone . t)))

  :demand t
  :config
  (progn

    ;; LaTeX-Beamer-PDF
    (defun ox-pandoc--pdf-engine ()
      "Set the default latex pdf engine to the one set by `org-latex-pdf-process'. "
      (let ((syms (mapcar (lambda (x) (cdr x)) halidom-latex-pdf-engines))
            (pred (lambda (sym) (eq (symbol-value sym) org-latex-pdf-process)))
            (sep "latex-process-"))
        (cadr (split-string (symbol-name (car (seq-filter pred syms))) sep))))

    (setq org-pandoc-options-for-beamer-pdf
          `((pdf-engine . ,(ox-pandoc--pdf-engine)))
          org-pandoc-options-for-latex-pdf
          `((pdf-engine . ,(ox-pandoc--pdf-engine))))

    (defun org-pandoc-pdf-engine-set (&optional process)
      "Set the latex pdf engine for `org-pandoc-export-to-latex-pdf'."
      (interactive)
      (let* ((process (or process
                          (assoc
                           (completing-read "Pandoc Process: "
                                            halidom-latex-pdf-engines nil nil)
                           halidom-latex-pdf-engines)))
             (sym (substring-no-properties (car process))))
        (setq org-pandoc-options-for-beamer-pdf
              `((pdf-engine . ,sym))
              org-pandoc-options-for-latex-pdf
              `((pdf-engine . ,sym)))))

    ;; Open MS .doc?x files with system viewer.
    (when (symbolp 'org-file-apps)
      (add-to-list 'org-file-apps '("\\.docx?\\'" . system)))))
;; Block = @181510
(message "org-dotemacs: evaluating @181510 block")
(use-package ox-gfm
  :after (ox)
  :demand t)
;; Block = @181696
(message "org-dotemacs: evaluating @181696 block")
(use-package ox-hugo
  :after (ox))
;; Block = @181795
(message "org-dotemacs: evaluating @181795 block")
(use-package org-html-themes
    :straight
    (org-html-themes
      :host github
      :repo "fniessen/org-html-themes"
      :local-repo-name org-html-themes
      :files ("setup/*" "styles/*")))
;; Block = @182239
(message "org-dotemacs: evaluating @182239 block")
  (defun org-occur-open (uri)
    "Visit the file specified by URI, and run `occur' on the fragment
    (anything after the first '#') in the uri."
    (let ((list (split-string uri "#")))
      (org-open-file (car list) t)
      (occur (mapconcat 'identity (cdr list) "#"))))
  (org-add-link-type "occur" 'org-occur-open)
;; Block = @182796
(message "org-dotemacs: evaluating @182796 block")
(defun org-mode-save-place-fix ()
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-show-subtree))))

(add-hook 'org-mode-hook 'org-mode-save-place-fix)
;; Block = @183144
(message "org-dotemacs: evaluating @183144 block")
  (require 'ibuffer)

  (defun org-ibuffer ()
    "Open an `ibuffer' window showing only `org-mode' buffers."
    (interactive)
    (ibuffer nil "*Org Buffers*" '((used-mode . org-mode))))
;; Block = @183396
(message "org-dotemacs: evaluating @183396 block")
(use-package org-protocol
    :straight org
    :custom
    (org-protocol-default-template-key "l")
    :init
    (require 'server)
    (unless (server-running-p)
      (setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
      (server-start))
    :config
    (require 'org-capture)
    (add-to-list 'org-capture-templates
                 `("l" "Protocol Link" entry
                   (file+headline ,org-default-notes-file "Inbox")
                   "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")))
;; Block = @184029
(message "org-dotemacs: evaluating @184029 block")
(use-package org-mac-protocol
    :straight (org-mac-protocol
               :type git
               :host github
               :repo "jchaffin/org-mac-protocol")

    :init
    (defun org-mac-protocol-install (&optional force)
      "Install system dependencies provided by org-mac-protocol."
      (interactive "P")
      (cl-flet
          ((add-capture-templates ()
             (when (require 'org-capture nil t)
               (setq org-capture-templates
                     (append org-capture-templates
                             (list `("y" "AppleScript capture" entry
                                         (file+headline ,org-default-notes-file "Capture")
                                         "** %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?")
                                   `("z" "AppleScript note" entry
                                         (file+headline ,org-default-notes-file "Notes")
                                         "** %?\n\n  Date: %u\n")))))))
        (lexical-let* ((target-directory "~/Library/Scripts/org-mac-protocol")
                       (source-directory (straight-repos-dir "org-mac-protocol"))
                       (scripts (directory-files source-directory t "\\.scpt$\\|orgQSLib"))
                       (force-p (if force t nil)))
          (unless (file-directory-p target-directory)
            (mkdir target-directory))

          (cl-loop for script in scripts
             for target = (expand-file-name (basename script)
                                            target-directory)
             do (condition-case nil
                    (progn
                      (make-symbolic-link script target force-p))
                  (error nil)))

          (if (f-empty-p target-directory)
              (warn "Org mac protocol scripts failed to install.")
            (add-capture-templates)
            (message "Scripts installed to %s" target-directory)))))
    :init
    (add-to-list 'org-modules 'org-mac-protocol))
;; Block = @186119
(message "org-dotemacs: evaluating @186119 block")
(use-package org-protocol-capture-html
  :straight (org-protocol-capture-html
             :host github
             :repo "alphapapa/org-protocol-capture-html")
  :after (org-capture)
  :init
  (add-to-list 'org-capture-templates
               '("w" "Web site" entry
                 (file "~/Dropbox/org/capture.org")
                   "* %a :website:\n\n%U %?\n\n%:initial"))

  (defun opch-install-shell-script ()
    (interactive)
    (let* ((opch-shell-script-path
            (emacs-var-dir
             "org-protocol-capture-html"
             "org-protocol-capture-html.sh"))
           (opch-directory (file-name-directory opch-shell-script-path)))

    (unless (file-directory-p opch-directory)
      (mkdir opch-directory))

    (unless (file-exists-p opch-shell-script-path)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/alphapapa/org-protocol-capture-html/master/org-protocol-capture-html.sh"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (when (re-search-backward "^\\#!/bin/bash" (point-min) t)
          (write-region (point) (point-max) opch-shell-script-path)
          (set-file-modes opch-shell-script-path #o755)
          (message "Installed to %s" opch-shell-script-path)))))))

;; Block = @187502
(message "org-dotemacs: evaluating @187502 block")
(use-package org-contacts
  :straight org
  :init
  (require 'org-contacts)
  (push '("E" "Contacts" entry (file "~/Dropbox/org/contacts.org")
           "* %(org-contacts-template-name)
              :PROPERTIES:
              :EMAIL: %(org-contacts-template-email)
              :END:")
        org-capture-templates))
;; Block = @187982
(message "org-dotemacs: evaluating @187982 block")
(use-package org-download
  :defines (org-download-image-dir)
  :commands (org-download-enable  org-download-yank org-download-screenshot)
  :init
  (when (eq system-type 'darwin)
    (setq-default org-download-image-dir "~/Dropbox/org/img/"))
  :hook
  ((dired-mode org-mode) . org-download-enable))
;; Block = @188342
(message "org-dotemacs: evaluating @188342 block")
(use-package org-web-tools)
;; Block = @188426
(message "org-dotemacs: evaluating @188426 block")
(use-package org-preview-html)
;; Block = @188703
(message "org-dotemacs: evaluating @188703 block")
(defcustom halidom--theme-style 'dark
  "The default theme mode. Only symbols 'dark and 'light are considered."
  :type '(choice
          (const :tag "Use a dark theme by default." dark)
          (const :tag "Use a light theme by default" light)
          (const :tag "Ignore the value of this variable" nil)))

(defcustom halidom-term-theme 'wombat
  "The default term theme."
  :type 'symbol)

(defcustom halidom-prose-theme 'tsdh-light
   "The default theme for writing text."
   :type 'symbol)

(defcustom halidom-light-theme 'tsdh-light
  "The light theme."
  :type 'symbol)

(defcustom halidom-dark-theme 'tsdh-dark
  "The dark theme."
  :type 'symbol)

(defcustom halidom-theme 'tsdh-dark
  "The default theme to load."
  :type 'string)

(defcustom halidom-theme-colors nil
  "plist of colors for the current theme."
  :type '(plist :key-type symbol :value-type sexp))
;; Block = @189734
(message "org-dotemacs: evaluating @189734 block")
(setq custom-safe-themes t)
(setq custom-theme-directory (emacs-etc-dir "themes"))
;; Block = @189904
(message "org-dotemacs: evaluating @189904 block")
(use-package zenburn-theme
  :straight t)
;; Block = @190083
(message "org-dotemacs: evaluating @190083 block")
(use-package poet-theme
  :init
  (setq halidom-light-theme 'poet))
;; Block = @190293
(message "org-dotemacs: evaluating @190293 block")
(use-package base16-theme
    :demand t
    :init
    (setq halidom-dark-theme 'base16-snazzy
          halidom-light-theme 'base16-default-light)

     (require 'base16-snazzy-theme)
     (defun halidom--base16-p (&optional theme)
      (let ((theme (or theme halidom-theme "")))
        (string-prefix-p "base16-" (symbol-name theme))))

    (add-to-list 'custom-theme-load-path
                 (expand-file-name "straight/build/base16-theme"
                                   user-emacs-directory))
    :config
    (setq base16-distinct-fringe-background nil))
;; Block = @190934
(message "org-dotemacs: evaluating @190934 block")
(setq-default custom-enabled-themes (list halidom-theme))

(defun remove-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (mapc #'disable-theme custom-enabled-themes))

(defvar halidom/load-theme-hook nil
  "List of functions to run when `halidom/load-theme' is invoked.")

(defun halidom--default-theme ()
  (cl-flet ((default-theme-symbol ()
              (symbol-value
               `,(intern (concat "halidom-"
                                 (format "%s" (symbol-value 'halidom--theme-style))
                                 "-theme"))))
            (req-theme (theme)
                       (require
                        `,(intern (concat (symbol-name theme) "-theme")))))
    (let ((theme (default-theme-symbol)))
      (and  (req-theme theme) theme))))

(cl-defun halidom/load-theme (&optional (theme (halidom--default-theme)))
  "Opionated theme loading. If invoked without arguments, this
function will load the theme associated with `halidom--theme-style'.

Calling this function with the universal-argument `C-u' loads the new
theme in \"quick mode\" by eliding hooks specified in
 `halidom/load-theme-hook'."

  (interactive
   (let ((themes (mapcar 'symbol-name (custom-available-themes))))
     (list
      (completing-read "Theme: " themes))))
  (interactive "p")

  (let ((theme-symbol (if (symbolp theme) theme (intern theme))))
    (remove-themes)
    (if (not window-system)
        (load-theme halidom-term-theme)
      (setq halidom-theme theme-symbol)
      (if (halidom--base16-p halidom-theme)
          (setq halidom-theme-colors
                (symbol-value
                 (intern
                  (eval
                   `(concat
                     ,(symbol-name (symbol-value 'halidom-theme))
                     "-colors")))))
        (setq halidom-theme-colors nil))
      (load-theme halidom-theme))
    (unless (equal current-prefix-arg '(4))
      (run-hooks 'halidom/load-theme-hook))))

(defun halidom/load-theme-with-frame (frame)
  (with-selected-frame frame
    (unless (daemonp)
      (halidom/load-theme))))

(defun halidom--toggle-theme-style ()
  (interactive)
  (if (eq halidom--theme-style 'dark)
      (setq halidom--theme-style 'light)
    (setq halidom--theme-style 'dark)))

(defun halidom/toggle-theme ()
  (interactive)
  (halidom--toggle-theme-style)
  (halidom/load-theme))

(add-hook 'after-init-hook #'halidom/load-theme)
;; (add-hook 'after-make-frame-functions #'halidom/load-theme-with-frame)
;; Block = @193564
(message "org-dotemacs: evaluating @193564 block")

;; Org Mode faces
(defun halidom/org-faces ()
  "Customize `org-mode' faces for base-16 themes."
  (let ((fg (face-foreground 'default nil 'default))
        (fg2 (or (plist-get halidom-theme-colors :base04)))
        (bg2 (or (plist-get halidom-theme-colors :base01)))
        (sans-font "Source Sans Pro")
        (header-font "EtBembo"))
    (if (and fg2 bg2)
        (progn
          (set-face-attribute 'org-document-info-keyword nil
                              :foreground fg2
                              :slant 'italic
                              :inherit 'org-document-info-face)
          (set-face-attribute 'org-block-begin-line nil
                              :height 1.0
                              :foreground fg2
                              :background bg2
                              :inherit 'org-meta-line))

      (set-face-attribute 'org-block-begin-line nil
                          :height 1.0
                          :inherit 'fixed-pitch
                          :background 'org-meta-line))

    (set-face-attribute 'org-level-2 nil
                        :family header-font
                        :height 1.2
                        :inherit 'outline-2)
    (set-face-attribute 'org-level-1 nil
                        :family sans-font
                        :height 1.6
                        :inherit 'outline-1)
    (set-face-attribute 'org-level-2 nil
                        :family sans-font
                        :height 1.3
                        :inherit 'outline-2)
    (set-face-attribute 'org-level-3 nil
                        :family sans-font
                        :height 1.2
                        :slant 'normal
                        :inherit 'outline-3)
    (set-face-attribute 'org-level-4 nil
                        :family sans-font
                        :height 1.1
                        :slant 'normal
                        :inherit 'outline-4)
    (set-face-attribute 'org-level-5 nil
                        :family sans-font
                        :height 1.1
                        :slant 'normal
                        :inherit 'outline-5)
    (set-face-attribute 'org-level-6 nil
                        :family sans-font
                        :height 1.1
                        :slant 'normal
                        :inherit 'outline-6)
    (set-face-attribute 'org-level-7 nil
                        :family sans-font
                        :height 1.0
                        :slant 'normal
                        :inherit 'outline-7)

    (set-face-attribute 'org-block nil
                        :height 1.0
                        :slant 'normal
                        :foreground fg)

    (set-face-attribute 'org-document-title nil
                        :family header-font
                        :foreground fg
                        :height 1.8
                        :underline nil
                        :inherit 'variable-pitch)

    (set-face-attribute 'org-block-end-line nil
                        :inherit 'org-block-begin-line)))

;; https://github.com/syl20bnr/spacemacs/pull/7667
(defun halidom/org-restart ()
  "Restart all open org-mode buffers."
  (let ((org-buffers (org-buffer-list)))
    (dolist (buf org-buffers)
      (with-current-buffer buf
        (org-mode-restart)
        (halidom/org-faces)
        (unless org-bullets-mode
          (org-bullets-mode 1))))))

(defun halidom/org-theme-hook ()
  (halidom/org-faces)
  (halidom/org-restart))

(defun halidom/disable-scroll-bar (&optional frame)
  (if frame
      (with-selected-frame frame
        (toggle-scroll-bar -1))
    (toggle-scroll-bar -1)))

;; Add Hooks
(add-hook 'halidom/load-theme-hook #'halidom/org-theme-hook)
(add-hook 'halidom/load-theme-hook #'halidom/disable-scroll-bar)
(add-hook 'after-make-frame-functions #'halidom/disable-scroll-bar)
;; Block = @197505
(message "org-dotemacs: evaluating @197505 block")
(use-package highlight)
;; Block = @197576
(message "org-dotemacs: evaluating @197576 block")
(use-package ov)
;; Block = @197651
(message "org-dotemacs: evaluating @197651 block")
(use-package ov-highlight
  :straight (ov-highlight
             :host github
             :repo "jkitchin/ov-highlight")
  :bind
  (:map org-mode-map
        ("C-c h" . ov-highlight/body))
  :init
  (require 'ov)
  (require 'ov-highlight))
;; Block = @197944
(message "org-dotemacs: evaluating @197944 block")
(use-package rainbow-mode
  :init
  (rainbow-mode 1))
;; Block = @198051
(message "org-dotemacs: evaluating @198051 block")
(use-package col-highlight)
;; Block = @198173
(message "org-dotemacs: evaluating @198173 block")
(use-package hl-todo
  :commands (hl-todo-mode)
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))
;; Block = @198422
(message "org-dotemacs: evaluating @198422 block")
(use-package symbol-overlay)
;; Block = @198548
(message "org-dotemacs: evaluating @198548 block")
(use-package highlight-sexp
  :straight t)
;; Block = @198676
(message "org-dotemacs: evaluating @198676 block")
(use-package highlight-symbol)
;; Block = @198888
(message "org-dotemacs: evaluating @198888 block")
(use-package highlight-indent-guides)
;; Block = @199012
(message "org-dotemacs: evaluating @199012 block")
(use-package highlight-indentation)

;; Block = @199127
(message "org-dotemacs: evaluating @199127 block")
(use-package visual-indentation-mode)
;; Block = @199303
(message "org-dotemacs: evaluating @199303 block")
(use-package pretty-mode
  :init
  (global-pretty-mode t)

  :config
  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple
               :arrows :arrows-twoheaded :punctuation
               :logic :sets))
  (pretty-activate-groups '(:greek)))
;; Block = @199933
(message "org-dotemacs: evaluating @199933 block")
  (when (display-graphic-p)
    (add-hook 'prog-mode-hook 'prettify-symbols-mode))
;; Block = @200095
(message "org-dotemacs: evaluating @200095 block")
(use-package prettify-utils
  :straight (prettify-utils
             :host github
             :repo "Ilazki/prettify-utils.el")
  :init
  (require 'prettify-utils))

;; Block = @200317
(message "org-dotemacs: evaluating @200317 block")
(use-package pretty-outlines
  :straight nil
  :load-path "etc/local/pretty-outlines"
  :init
  (defalias #'package-installed-p #'straight-package-installed-p
    "If non-nil then PKG is installed.")
  :custom
  (pretty-outlines-ellipsis " ")
  :hook ((outline-mode       . pretty-outlines-set-display-table)
         (outline-minor-mode . pretty-outlines-set-display-table)
         (emacs-lisp-mode . pretty-outlines-add-bullets)
         (python-mode     . pretty-outlines-add-bullets)))


;; Block = @200863
(message "org-dotemacs: evaluating @200863 block")
(use-package pretty-fonts
  :load-path "etc/local/pretty-fonts"
  :straight nil
  :init
  (defun pretty-fonts/setup ()
    (require 'pretty-fonts)
    "Setup fira code ligatures and icon font sets."
    (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
    (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

    (pretty-fonts-set-fontsets-for-fira-code)
    (pretty-fonts-set-fontsets
     '(;; All-the-icons fontsets
       ("fontawesome"
        ;;                         
        #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

       ("all-the-icons"
        ;;    
        #xe907 #xe928)

       ("github-octicons"
        ;;                               
        #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

       ("material icons"
        ;;              
        #xe871 #xe918 #xe3e7  #xe5da
        ;;              
        #xe3d0 #xe3d1 #xe3d2 #xe3d4))))
  :hook
  (halidom/load-theme . pretty-fonts/setup))
;; Block = @201899
(message "org-dotemacs: evaluating @201899 block")

(use-package pretty-code
  :load-path "etc/local/pretty-code"
  :straight nil
  :init
  (require 'pretty-code)
  (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")))
  (pretty-code-add-hook 'hy-mode-hook         '((:def "defn")
                                                    (:lambda "fn")))
  (pretty-code-add-hook 'python-mode-hook     '((:def "def")
                                                (:lambda "lambda"))))
;; Block = @202622
(message "org-dotemacs: evaluating @202622 block")
(setq use-default-font-for-symbols nil)
;; Block = @202714
(message "org-dotemacs: evaluating @202714 block")
(defcustom user-fonts-list
  '(("Fira Code" . nil)
    ("Fira Mono for Powerline" . nil)
    ("Operator Mono" . extralight)
    ("SF Mono" . normal)
    ("Ubuntu Mono" . nil)
    ("Monaco" . nil)
    ("Inconsolota" . nil))

  "The default font stack to use for setting the font
  on startup and new frame."
  :type '(string))

(defun font/setup ()
  (interactive)
  (let* ((font-family (or (car (seq-intersection
                               (mapcar #'car user-fonts-list) (font-family-list)))
                         (face-attribute 'fixed-pitch :family)))
         (weight (or (cdr (assoc font-family user-fonts-list)) 'normal)))
    (run-at-time "0.2 sec" nil
                 `(lambda ()
                    (when (not (eq (face-attribute 'default :family)
                                 ,font-family))
                      (set-face-attribute
                       'default nil
                       :family ,font-family
                       :weight (quote ,weight)
                       :height 120))))))

(add-hook 'after-init-hook 'font/setup)
;; Block = @203832
(message "org-dotemacs: evaluating @203832 block")
(use-package org-variable-pitch
  :straight (org-variable-pitch
             :type git
             :host github
             :repo "emacsmirror/org-variable-pitch")
  :init
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  :hook
  (org-mode . org-variable-pitch-minor-mode))
;; Block = @204289
(message "org-dotemacs: evaluating @204289 block")

;; Block = @204370
(message "org-dotemacs: evaluating @204370 block")
  (use-package unicode-fonts
    :init
    (unicode-fonts-setup))
;; Block = @204507
(message "org-dotemacs: evaluating @204507 block")
(when (eq system-type 'darwin)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
;; Block = @204663
(message "org-dotemacs: evaluating @204663 block")
(use-package unicode-emoticons)
;; Block = @204772
(message "org-dotemacs: evaluating @204772 block")
(use-package company-emoji
  :after (:all company emojify-mode)
  :demand t
  :init
    ;; https://github.com/dunn/company-emoji#emoji-font-support
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        ;; For NS/Cocoa
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
  (--set-emoji-font nil)

  ;; Hook for when a frame is created with emacsclient
  (add-hook 'after-make-frame-functions '--set-emoji-font)

  :config
  (defun company-emoji-setup ()
    (let ((backends (cons 'company-emoji company-backends)))
      (set (make-local-variable 'company-backends) backends)))

  (add-hook 'emojify-mode-hook #'company-emoji-setup))

;; Block = @205818
(message "org-dotemacs: evaluating @205818 block")
(use-package all-the-icons
    :demand t)
;; Block = @205922
(message "org-dotemacs: evaluating @205922 block")
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :demand t
  :init
  (require 'font-lock+)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;; Block = @206147
(message "org-dotemacs: evaluating @206147 block")
  (use-package all-the-icons-ivy
    :after (:all ivy all-the-icons)
    :init
    (all-the-icons-ivy-setup))

;; Block = @206391
(message "org-dotemacs: evaluating @206391 block")
(defun fonts/setup-glyphs ()
  "Modify the default fontset to choose the right icons."
  (let ((font-glyphs-alist '(("github-octicons" . (#xF0C5 #xF02F #xF00E))
                             ("file-icons" . (#xE917  #xE600))
                             ("Material Icons" . (#xE80C #xE8CE))))
        (font-families (font-family-list)))
    (dolist (font-glyphs font-glyphs-alist)
      (when (member (car font-glyphs) font-families)
        (mapcar
         (lambda (glyph)
           (set-fontset-font "fontset-default"
                             glyph (car font-glyphs)
                             nil 'prepend))
         (cdr font-glyphs))))))


(add-hook 'after-init-hook 'fonts/setup-glyphs)

;; Block = @207149
(message "org-dotemacs: evaluating @207149 block")
;; Unicode Fonts

;; Emojis

;; Block = @207321
(message "org-dotemacs: evaluating @207321 block")
(use-package vscode-icon
  :init
  (require 'vscode-icon)
  :commands (vscode-icon-for-file))

;; Block = @207610
(message "org-dotemacs: evaluating @207610 block")
(blink-cursor-mode -1)
;; Block = @207730
(message "org-dotemacs: evaluating @207730 block")
(setq-default cursor-in-non-selected-windows nil
              x-stretch-cursor nil)
;; Block = @207896
(message "org-dotemacs: evaluating @207896 block")
(use-package cursor-chg
  :config
  (require 'cursor-chg)
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1))
;; Block = @208257
(message "org-dotemacs: evaluating @208257 block")
(use-package visual-fill-column
  :custom
  (visual-fill-column-width
   ;; take Emacs 26 line numbers into account
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column))
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (defun org-visual-fill-column ()
      (visual-fill-column-mode 1)
      (visual-line-mode 1))
  :hook
  (org-mode . org-visual-fill-column))
;; Block = @208820
(message "org-dotemacs: evaluating @208820 block")
(use-package fill-column-indicator
  :init
  (setq fci-rule-use-dashes nil))
;; Block = @209039
(message "org-dotemacs: evaluating @209039 block")
(use-package justify-kp
  :straight (:host github
                   :repo "Fuco1/justify-kp"))
;; Block = @209304
(message "org-dotemacs: evaluating @209304 block")
(use-package spaceline)
;; Block = @209392
(message "org-dotemacs: evaluating @209392 block")
(use-package spaceline-all-the-icons
  :after (:all spacemacs all-the-icons))

(defun ml-spaceline-setup ()
  (interactive)
  "Setup the modeline for spaceline."
  (setq spaceline-all-the-icons-separator-type 'arrow)
  (require 'spaceline)
  (require 'spaceline-segments)
  (require 'spaceline-config)
  (require 'all-the-icons)
  (require 'spaceline-all-the-icons)
  (spaceline-all-the-icons--setup-neotree)
  (spaceline-all-the-icons-theme 'mu4e-alert 'org-pomodoro)
  (spaceline-toggle-all-the-icons-flycheck-status-off))

  (add-hook 'after-init-hook #'ml-spaceline-setup)


;; Block = @210132
(message "org-dotemacs: evaluating @210132 block")
(use-package simple-httpd
    :straight (simple-httpd
               :type git
               :host github
               :repo "skeeto/emacs-web-server"
               :local-repo "simple-httpd"))
;; Block = @210401
(message "org-dotemacs: evaluating @210401 block")
(use-package websocket)
;; Block = @210486
(message "org-dotemacs: evaluating @210486 block")
(use-package uuid)
;; Block = @210577
(message "org-dotemacs: evaluating @210577 block")
(use-package web-server)
;; Block = @210669
(message "org-dotemacs: evaluating @210669 block")
(use-package request)
;; Block = @210755
(message "org-dotemacs: evaluating @210755 block")
(use-package oauth2)
;; Block = @210963
(message "org-dotemacs: evaluating @210963 block")
(use-package browse-url
    :custom (browse-url-chromium-program
             (if (eq system-type 'darwin)
                 "/Applications/Chromium.app/Contents/MacOS/Chromium"
               "chromium"))
  :config
  (progn
    (when (not (display-graphic-p))
      (setq browse-url-browser-function 'eww-browse-url))))

;; Block = @211503
(message "org-dotemacs: evaluating @211503 block")
(use-package osx-browse
  :if (eq system-type 'darwin)
  :defines (osx-browse-mode osx-browse-mode-map)
  :demand t
  :init
  (progn
    (osx-browse-mode 1)))
;; Block = @211738
(message "org-dotemacs: evaluating @211738 block")
(use-package google-this
  :init
  (google-this-mode 1)
  :config
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c /" " This")
    (push '(( nil . "google-this-\\(.+\\)") . (nil . "this \\1"))
          which-key-replacement-alist)))
;; Block = @212079
(message "org-dotemacs: evaluating @212079 block")
  (use-package search-web)
;; Block = @212161
(message "org-dotemacs: evaluating @212161 block")
(use-package xwidget
  :straight nil
  :bind (:map xwidget-webkit-mode-map
              ("\C-s" . isearch-forward)
              ("d" . xwidget-webkit-download))

  :defines (xwidget-webkit-current-url-as-kill xwidget-webkit-download)
  :init
  (defadvice xwidget-webkit-current-url-message-kill
      (around xwidget-webkit-current-url-message-kill-advice activate)
    (interactive)
    (let ((url (funcall-interactively #'xwidget-webkit-current-url)))
      (kill-new url)
      (message "Copied to kill ring: %s" url)))

  (when (require 'eww nil t)
    (defalias #'xwidget-webkit-decode-url-file-name #'eww-decode-url-file-name)
    (defalias #'xwidget-make-unique-file-name #'eww-make-unique-file-name)

    (defun xwidget-webkit-download (directory)
      "Download the current file from the url visited in the
active xwidget buffer and write to file in DIRECTORY."
      (interactive
       (list (read-directory-name
              "Download to: "
              xwidget-webkit-download-dir  nil nil)))
      (let ((url (funcall-interactively #'xwidget-webkit-current-url))
            (default-directory directory))
        (url-retrieve url #'xwidget-webkit-download-callback (list url directory))))

    (defun xwidget-webkit-download-callback (status url directory)
      (unless (plist-get status :error)
        (let* ((obj (url-generic-parse-url url))
               (path (car (url-path-and-query obj)))
               (file (eww-make-unique-file-name
                      (eww-decode-url-file-name (file-name-nondirectory path))
                      directory)))
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point) (point-max) file))
          (message "Saved %s" file)))))

  :config
  (defun xwidget-webkit-open-file (&optional file)
    "Open local FILE in"
    (interactive "fFile: ")
    (xwidget-webkit-browse-url
     (concat "file://" (copy-file-path)))))
;; Block = @214260
(message "org-dotemacs: evaluating @214260 block")
(defvar browse-url-browser-alist
      '(("chrome" . browse-url-chrome)
        ("firefox" . browse-url-firefox)
        ("eww" . eww-browse-url)
        ("xwidget-webkit" . xwidget-webkit-browse-url)))

(if (eq system-type 'darwin)
    (progn
      (setf (cdr (assoc "chrome" browse-url-browser-alist))
            #'osx-browse-url-chrome)
      (setf (cdr (assoc "firefox" browse-url-browser-alist))
            #'osx-browse-url-firefox)
      (add-to-list 'browse-url-browser-alist
                   '("safari" . osx-browse-url-safari))))

(defun set-browser-function (browser)
  "Interactively set the browser used by `browse-url'. "
  (interactive
   (list (completing-read "Browser: " browse-url-browser-alist)))
  (when-let ((browser-function (cdr (assoc browser browse-url-browser-alist))))
    (setq browse-url-browser-function browser-function)))

(cl-defun make-browser-function (browser)
  `(defun ,(intern (concat "set-browser-function-" browser)) ()
     (interactive)
     (set-browser-function ,browser)))

(defmacro make-browser-functions (browsers)
  `(progn ,@(mapcar 'make-browser-function browsers)))


(eval `(make-browser-functions ,(mapcar 'car browse-url-browser-alist)))

;; Block = @215533
(message "org-dotemacs: evaluating @215533 block")
(use-package engine-mode
  :init
  (engine-mode t)

  :config
  (setq web-engine-prefix nil)

  (when (and (boundp 'engine/keybinding-prefix) web-engine-prefix)
    (define-key engine-mode-map (kbd "C-x /") nil))

  (defengine cassi
    "http://cassi.cas.org/search.jsp")

  (defengine ctan
    "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s"
    :keybinding "c"
    :docstring "Search the Comprehensive TeX Archive Network (ctan.org)")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h"
    :docstring "Search Github")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"
    :docstring "Search Google")

  (defengine google-bookmarks
    "chrome://bookmarks/?q=%s"
    :keybinding "Gb"
    :docstring "Search bookmarks in Google Chrome")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "Gi"
    :docstring "Search google-images")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "Gm"
    :docstring "Mappin' it up.")

  (defengine google-schoolar
    "http://scholar.google.com/scholar?q=%s"
    :keybinding "Gs"
    :docstring "Schoolin'it up.")

  (defengine project-gutenberg
    "http://www.gutenberg.org/ebooks/search/?query=%s"
    :docstring "Read good")

  (defengine proquest
    "https://search.proquest.com"
    :keybinding "p"
    :docstring "Search Proquest")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s"
    :docstring "Search StackOverflow.")

  (defengine twitter
    "https://twitter.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Search wikipedia.")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :keybinding "a"
    :docstring "Search Wolfram Alfa")

  (defengine worldcat
    "https://ucla.worldcat.org/search?q=%s"
    :keybinding "W"
    :docstring "Search Worldcat"
    :browser 'osx-browse-url-chrome)

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"
    :docstring "Search Youtube"))
;; Block = @218079
(message "org-dotemacs: evaluating @218079 block")
(use-package org-mime
  :config
  (setq org-mime-export-options
        '(:section-numbers nil
          :with-author nil
          :with-toc nil))


  (defun org-mime/style ()
    (org-mime-change-element-style
     "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                   "#E6E1DC" "#232323"))
    (org-mime-change-element-style
     "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))

  (defun org-mime/message-mode-hook ()
    (local-set-key (kbd "C-c M-h") 'org-mime-htmlize))

  (defun org-mime/org-mode-hook ()
      (local-set-key (kbd  "C-c M-h") 'org-mime-org-buffer-htmlize))

  :hook
  (message-mode . org-mime/message-mode-hook)
  (org-mode     . org-mime/org-mode-hook)
  (org-mime-html . org-mime/style))

;; Block = @218910
(message "org-dotemacs: evaluating @218910 block")
(use-package offlineimap)
;; Block = @219004
(message "org-dotemacs: evaluating @219004 block")
  (use-package mu4e
      :straight nil
      :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
      :init
      (require 'mu4e)

      :config
      ;; use mu4e for email in emacs
      (setq mail-user-agent 'mu4e-user-agent)

      (setq mu4e-maildir "~/.mail")

      ;; Update with offlineimap
      (setq mu4e-get-mail-command "offlineimap -o -q")

      ;; Let GMAIL/IMAP take care of sent messages
      (setq mu4e-sent-messages-behavior 'delete)

      (require 'smtpmail)

       ;; smtp settings
      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      ;; Kill message buffers on send
      (setq message-kill-buffer-on-exit t)

      ;; mu4e View
      (setq mu4e-attachment-dir "~/Downloads"
            mu4e-view-show-images t
            mu4e-show-addresses t
            mu4e-view-prefer-html t
            mu4e-view-show-images t)


      (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))

      (setq mu4e-completing-read-function 'ivy-completing-read)

      ;; Mu4e Contexts
      (setq mu4e-context-policy 'pick-first
            mu4e-compose-context-policy 'ask-if-none)

      (setq mu4e-contexts
            `(,(make-mu4e-context
                :name "personal"
                :enter-func
                (lambda () (mu4e-message "Using Personal gmail account."))
                :leave-func
                (lambda () (mu4e-message "leaving Personal gmail account."))
                :match-func
                (lambda (msg)
                  (when msg
                    (string-match-p "^/personal")
                    (mu4e-message-field msg :maildir)))
                :vars
                '((user-mail-address  . "jchaffin57@gmail.com")
                  (user-full-name     . "Jacob Chaffin")
                  (mail-reply-to      . "jchaffin57@gmail.com")
                  (smtpmail-smtp-user . "jchaffin57@gmail.com")
                  (mu4e-sent-folder   . "/personal/sent")
                  (mu4e-drafts-folder . "/personal/drafts")
                  (mu4e-refile-folder . "/personal/archive")
                  (mu4e-trash-folder  . "/personal/trash")
                  (mu4e-compose-signature .
                   (concat
                    "Jacob Chaffin\n"
                    "jchaffin57@gmail.com\n"
                    "jchaffin@ucla.edu"))))

               ,(make-mu4e-context
                 :name "school"
                 :enter-func
                 (lambda () (mu4e-message "Entering UCLA Gmail account"))
                 :leave-func
                 (lambda () (mu4e-message "Leaving UCLA Gmail account."))
                 :match-func
                 (lambda (msg)
                   (when msg
                     (string-match-p "^/school"
                     (mu4e-message-field msg :maildir))))
                 :vars '((user-mail-address  . "jchaffin@ucla.edu")
                         (user-full-name     . "Jacob Chaffin")
                         (mail-reply-to      . "jchaffin@ucla.edu" )
                         (smtpmail-smtp-user . "jchaffin@g.ucla.edu")
                         (mu4e-drafts-folder . "/school/drafts")
                         (mu4e-sent-folder   . "/school/sent")
                         (mu4e-refile-folder . "/school/archive")
                         (mu4e-trash-folder  . "/school/trash")
                         (mu4e-compose-signature .
                          (concat
                           "Jacob Chaffin\n"
                           "UCLA 2019\n"
                           "Linguistics and Computer Science\n"
                           "jchaffin@ucla.edu"))))))

      ;; From [[https://www.djcbsoftware.nl/code/mu/mu4e/Some-context-tricks.html#Some-context-tricks][9.5 - Some Context Tricks]]
      ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
      ;; `user-mail-address' values for all contexts. If you have other mail
      ;; addresses as well, you'll need to add those manually.
      (setq mu4e-user-mail-address-list
            (delq nil
                  (mapcar (lambda (context)
                            (when (mu4e-context-vars context)
                              (cdr
                               (assq
                                'user-mail-address
                                (mu4e-context-vars context)))))
                          mu4e-contexts)))


      (require 'gnus-dired)

      (defun gnus-dired-mail-buffers ()
        "Return a list of active message buffers."
        (let ((buffers))
          (save-current-buffer
            (dolist (buffer (buffer-list t))
              (set-buffer buffer)
              (when (and (derived-mode-p 'message-mode)
                         (null message-sent-message-via))
                (push (buffer-name buffers) buffers))))
          (nreverse buffers)))

      (setq gnus-dired-mail-mode 'mu4e-user-agent)

      (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))
;; Block = @224209
(message "org-dotemacs: evaluating @224209 block")
(use-package mu4e-alert
    :config
    (mu4e-alert-enable-notifications))
;; Block = @224368
(message "org-dotemacs: evaluating @224368 block")
(use-package mu4e-maildirs-extension
    :after (mu4e)
    :init
    (mu4e-maildirs-extension-load))
;; Block = @224525
(message "org-dotemacs: evaluating @224525 block")
(use-package mu4e-conversation)
;; Block = @224675
(message "org-dotemacs: evaluating @224675 block")
(use-package debbugs
  :straight (debbugs
             :type git
             :repo "https://git.savannah.gnu.org/git/emacs/elpa.git"
             :files ("packages/debbugs/*.el"
                     "packages/debbugs/Debbugs.wsdl")
             :local-repo "elpa"))

;; Block = @225084
(message "org-dotemacs: evaluating @225084 block")
(use-package browse-at-remote
  :init
  (if (eq system-type 'darwin)
      (when (fboundp 'osx-browse-url-chrome)
        (setq browse-url-browser/function 'osx-browse-url-chrome))))
;; Block = @225508
(message "org-dotemacs: evaluating @225508 block")
(use-package deft
  :if (eq system-type 'darwin)
  :bind ("C-x C-n" . deft)
  :custom
  (deft-auto-save-interval 0)
  (deft-extensions '("org"))
  (deft-directory "~/Dropbox/org/notes/")
  (deft-use-filename-as-title t)
  (deft-default-extension "org"))
;; Block = @225944
(message "org-dotemacs: evaluating @225944 block")
(use-package org-onenote
  :init
  (require 'secrets)
  :custom
  (org-onenote-token-file
   (emacs-etc-dir "org-onenote-oauth2.plstore")))
;; Block = @226161
(message "org-dotemacs: evaluating @226161 block")
(use-package lorem-ipsum
  :straight t)
;; Block = @226276
(message "org-dotemacs: evaluating @226276 block")
  (use-package org-velocity
    :straight org
    :bind
    (("C-c n" . org-velocity))
    :demand t
    :custom
    (org-velocity-bucket (expand-file-name "bucket.org" org-directory))
    :init
  (require 'org-velocity))
;; Block = @226663
(message "org-dotemacs: evaluating @226663 block")
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :config
  (progn

    (require 'justify-kp)


    (defun nov-setup ()
      (face-remap-add-relative 'variable-pitch
                               :family "Bookmania")
      (visual-line-mode 1)
      (visual-fill-column-mode 1)

      (setq nov-text-width most-positive-fixnum
            visual-fill-column-center-text t))

    (defun my-nov-window-configuration-change-hook ()
      (my-nov-post-html-render-hook)
      (remove-hook 'window-configuration-change-hook
                   'my-nov-window-configuration-change-hook t))

    (defun my-nov-post-html-render-hook ()
      (if (get-buffer-window)
          (let ((max-width (pj-line-width))
                buffer-read-only)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (not (looking-at "^[[:space:]]*$"))
                  (goto-char (line-end-position))
                  (when (> (shr-pixel-column) max-width)
                    (goto-char (line-beginning-position))
                    (pj-justify)))
                (forward-line 1))))
        (add-hook 'window-configuration-change-hook
                  'my-nov-window-configuration-change-hook
                  nil t)))


    (add-hook 'nov-mode-hook 'nov-setup)
    (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)))
;; Block = @228128
(message "org-dotemacs: evaluating @228128 block")

(use-package olivetti
  :init
  (defun halidom/olivetti-setup ()
    (auto-fill-mode -1)
    (when (fboundp 'centered-cursor-mode)
      (centered-cursor-mode)))
  :hook
  (olivetti-mode . halidom/olivetti-setup))

;; Block = @228418
(message "org-dotemacs: evaluating @228418 block")
  (use-package writeroom-mode
    :config
    (defun halidom/writeroom-mode-hook ()
      (org-toggle-variable-pitch)
      (auto-fill-mode -1))
    (add-hook 'writeroom-mode #'halidom/writeroom-mode-hook))
;; Block = @228775
(message "org-dotemacs: evaluating @228775 block")
(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil)
  :init
  (require 'lsp)
  (require 'lsp-clients)
  :hook
  (lsp-after-open . lsp-enable-imenu))
;; Block = @228997
(message "org-dotemacs: evaluating @228997 block")
(use-package lsp-ui)

(use-package lsp-ui-peek
  :straight lsp-ui
  :bind
  (:map lsp-ui-peek-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)))
;; Block = @229314
(message "org-dotemacs: evaluating @229314 block")
(use-package company-lsp
    :after company
    :demand t
    :init
    (push 'company-lsp company-backends)
    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil))
;; Block = @229664
(message "org-dotemacs: evaluating @229664 block")
(use-package dap-mode
  :after lsp-mode
  :demand t
  :config
  (dap-mode t)
  (dap-ui-mode t))
;; Block = @229829
(message "org-dotemacs: evaluating @229829 block")
  (use-package flycheck
      :custom
      (flycheck-global-modes nil)
      (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
      (flycheck-emacs-lisp-load-path 'inherit)
      :init
      (defun halidom/flycheck-enable (mode)
        "Use flycheck in MODE."
        (push mode flycheck-global-modes))

      (defun halidom/flycheck-enable-hook ()
        "Enable Flycheck as a hook."
        (when (fboundp 'flycheck-mode)
          (flycheck-mode +1)))

      (defun halidom/toggle-flycheck-error-list ()
        "Toggle flycheck's error list window.
  If the error list is visible, hide the window.
  Else display the buffer."
        (interactive)
        (-if-let (window (flycheck-get-error-list-window))
            (quit-window nil window)
          (flycheck-list-errors)))

      (defun halidom/goto-flycheck-error-list ()
        "Open and go to the error list buffer."
        (interactive)
        (unless (get-buffer-window
                 (get-buffer flycheck-error-list-buffer))
          (flycheck-list-errors)
          (switch-to-buffer-other-window
           flycheck-error-list-buffer))))
;; Block = @231049
(message "org-dotemacs: evaluating @231049 block")
(setq-default tab-width 2
              indent-tabs-mode nil)
;; Block = @231196
(message "org-dotemacs: evaluating @231196 block")
  ; Line Numbering
  (when (>= emacs-major-version 26)
    (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; Block = @231381
(message "org-dotemacs: evaluating @231381 block")
(use-package vimish-fold
  :init
  (vimish-fold-global-mode 1))
;; Block = @231522
(message "org-dotemacs: evaluating @231522 block")
(use-package hideshow
  :straight nil)
;; Block = @231619
(message "org-dotemacs: evaluating @231619 block")
(use-package hideshowvis
  :init
  (defface halidom-folded-face
      `((((background light)) :background ,(plist-get halidom-theme-colors :base00))
        (((background dark)) :background ,(plist-get halidom-theme-colors :base03)))
    "Face to highlight `hideshow' overlays.")

  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (when (featurep 'vimish-fold)
              (overlay-put
               ov 'before-string
               (propertize "..." 'display
                           (list vimish-fold-indication-mode
                                 'empty-line
                                 'vimish-fold-fringe))))
            (overlay-put
             ov 'display (propertize " [...] " 'face 'halidom-folded-face)))))

  (defun hideshowvis/enable ()
    (hideshowvis-minor-mode 1))
  :hook
  (prog-mode . hideshowvis/enable))

;; Block = @232568
(message "org-dotemacs: evaluating @232568 block")
(use-package hideshow-org
  :bind
  ("C-c h" . hs-org/minor-mode)

  :hook
  (org-mode . hs-org/minor-mode))
;; Block = @232729
(message "org-dotemacs: evaluating @232729 block")
;; Editorconfig
(use-package editorconfig
  :if (executable-find "editorconfig")
  :init (editorconfig-mode 1))
;; Block = @232923
(message "org-dotemacs: evaluating @232923 block")
(use-package google-c-style
  :straight
  (google-c-style
   :host github
   :repo "google/styleguide"
   :branch "gh-pages")
  :init
  (defun halidom/google-c-style ()
    (google-set-c-style)
    (google-make-newline-indent))
  :hook
  (c++-mode . halidom/google-c-style)
  (java-mode . halidom/google-c-style))
;; Block = @233315
(message "org-dotemacs: evaluating @233315 block")
(use-package electric-operator)
;; Block = @233423
(message "org-dotemacs: evaluating @233423 block")
(use-package format-all)
;; Block = @233492
(message "org-dotemacs: evaluating @233492 block")
(use-package eldoc
  :config
  (when (fboundp 'paredit)
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)))
;; Block = @234050
(message "org-dotemacs: evaluating @234050 block")
(use-package paredit
  :defines (enable-paredit-mode)
  :hook
  ((emacs-lisp-mode clojure-mode lisp-mode) . enable-paredit-mode))


;; Block = @234746
(message "org-dotemacs: evaluating @234746 block")
(use-package smartparens
  :custom-face
  (sp-show-pair-match-face
   ((t (:underline
        (:color foreground-color :style line)
        :inherit (show-paren-match)))))
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :custom
  (sp-show-pair-delay
   (or (bound-and-true-p sp-show-pair-delay) 0.2))
  (sp-show-pair-from-inside t)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  :init
  (defun smartparens-pair-newline (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode)))

  (defun smartparens-pair-newline-and-indent (id action context)
    (smartparens-pair-newline id action context)
    (indent-according-to-mode))

  (defun conditionally-enable-smartparens-mode ()
    "enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-mode)))


  (defun smartparens-adaptive-overlay-face ()
    (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil))


  (defun sp-wrap-inline-math ()
    "Wrap marked region as ordinary LaTeX inline math mode."
    (interactive)
    (sp-wrap-with-pair "$"))

  (defun disable-smartparens ()
      "Disable smartparens when `paredit-mode' is enabled."
    (smartparens-mode -1))

  :config
  (require 'smartparens-config)

  ;; enable globally
  (show-smartparens-global-mode +1)

  (smartparens-adaptive-overlay-face)

  (when (featurep 'paredit)
    (add-hook 'paredit-mode-hook #'disable-smartparens))

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (sp-pair "{" nil :post-handlers
           '(:add (smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (smartparens-pair-newline-and-indent "RET"))))
;; Block = @236722
(message "org-dotemacs: evaluating @236722 block")
(use-package rainbow-delimiters)
;; Block = @236823
(message "org-dotemacs: evaluating @236823 block")
(use-package parinfer
  :bind
  (("C-," . parinfer-toggle-mode))
  :custom
  (parinfer-extensions
   '(defaults smart-yank pretty-parens paredit)))

;; Block = @237040
(message "org-dotemacs: evaluating @237040 block")
(use-package lispy
  :config
  (when (functionp 'lispy-set-key-theme)
    (lispy-set-key-theme '(special paredit c-digits))))

;; Block = @237299
(message "org-dotemacs: evaluating @237299 block")
(use-package ggtags
  :if (and (getenv "GTAGSLABEL") (executable-find "global"))
  :custom (ggtags-highlight-tag nil))
;; Block = @237500
(message "org-dotemacs: evaluating @237500 block")
(use-package ediff
  :custom
  (ediff-diff-options "-w"))
;; Block = @237634
(message "org-dotemacs: evaluating @237634 block")
(use-package git-modes
  :straight t
  :mode (".projectile\\'" . gitignore-mode))
;; Block = @238286
(message "org-dotemacs: evaluating @238286 block")
(use-package magit
    :defines (magit-mode-hook)
    :bind
    (("C-c v v" . magit-status)
     ("C-c v m" . magit-merge)
     ("C-c v b" . magit-blame)
     ("C-c v C" . magit-clone)
     ("C-c v i" . magit-init)
     ("C-c v l" . magit-log-buffer-file)
     ("C-c v c" . magit-checkout)
     ("C-c v f" . magit-stage-file)
     ("C-c v p" . magit-pull)
     ("C-c v P" . magit-push)
     ("C-c v S" . magit-stash))

    :init
    (which-key-add-key-based-replacements "C-c v" " Magit")

    (push '(( nil . "magit-\\(.+\\)") . (nil . " \\1"))
          which-key-replacement-alist)
    :custom
    (magit-save-repository-buffers 'dontask)
    (magit-completing-read-function 'ivy-completing-read))
;; Block = @239166
(message "org-dotemacs: evaluating @239166 block")
(use-package magit-topgit
  :ensure-system-package tg
  :demand t
  :after (magit)
  :hook
  (magit-mode . turn-on-magit-topgit)
)
;; Block = @239462
(message "org-dotemacs: evaluating @239462 block")
(use-package magit-stgit
  :ensure-system-package stg
  :after (magit)
  :demand t
  :hook
  (magit-mode . magit-stgit-mode))
;; Block = @239642
(message "org-dotemacs: evaluating @239642 block")
(use-package magit-imerge
  :ensure-system-package git-imerge
  :after (magit)
  :demand t)
;; Block = @239905
(message "org-dotemacs: evaluating @239905 block")
(use-package magithub
  :after (magit)
  :commands magithub-dispatch-popup
  :bind (:map magit-status-mode-map
	      ("@" . magithub-dispatch-popup))
  :config
  (magithub-feature-autoinject t))
;; Block = @240158
(message "org-dotemacs: evaluating @240158 block")
(use-package magit-org-todos)
;; Block = @240291
(message "org-dotemacs: evaluating @240291 block")
(use-package orgit)
;; Block = @240566
(message "org-dotemacs: evaluating @240566 block")
(use-package gist
  :bind
  (("C-c C-g l" . gist-list)
   ("C-c C-g r" . gist-region)
   ("C-c C-g b" . gist-buffer)
   ("C-c C-g p" . gist-buffer-private)
   ("C-c C-g B" . gist-region-or-buffer)
   ("C-c C-g P" . gist-region-or-buffer-private))
  :init
  (push '(( nil . "gist-\\(.+\\)") . (nil . " \\1"))
        which-key-replacement-alist)

  (which-key-add-key-based-replacements
      "C-c C-g" " Gist"))
;; Block = @241104
(message "org-dotemacs: evaluating @241104 block")
(use-package git-timemachine
  :bind
  ("C-c v t" . git-timemachine-toggle)
  :config
  (setq git-timemachine-abbreviation-length 7)
  (which-key-add-key-based-replacements
    "C-c v t" " Timemachine"))
;; Block = @241434
(message "org-dotemacs: evaluating @241434 block")
(use-package git-messenger
  :bind
  ("C-c C-v m" . git-messenger:popup-message))
;; Block = @241602
(message "org-dotemacs: evaluating @241602 block")
(use-package git-gutter+
  :custom
  (git-gutter+-disabled-modes '(image-mode org-mode))
  :init
  (global-git-gutter+-mode))
;; Block = @241796
(message "org-dotemacs: evaluating @241796 block")
(use-package git-gutter-fringe+
  :hook
  (org-mode . git-gutter-fr+-minimal))
;; Block = @242070
(message "org-dotemacs: evaluating @242070 block")
(use-package monky
  :custom
  (monky-process-type 'cmdserver))
;; Block = @242193
(message "org-dotemacs: evaluating @242193 block")
(use-package ahg)
;; Block = @242266
(message "org-dotemacs: evaluating @242266 block")
(use-package ecloud
  :ensure-system-package
  (ecloud . azure-cli)
  :straight
  (ecloud :host github
          :type git
          :repo "techniumlabs/ecloud"))

;; Block = @242476
(message "org-dotemacs: evaluating @242476 block")
(use-package docker)
;; Block = @242574
(message "org-dotemacs: evaluating @242574 block")
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
;; Block = @242706
(message "org-dotemacs: evaluating @242706 block")
(use-package docker-compose-mode
    :mode ("docker-compose.yml\\'" . docker-compose-mode))

;; Block = @242861
(message "org-dotemacs: evaluating @242861 block")
(use-package aws
  :config
  (progn
    (autoload 'ec2-desribe-instances "aws")
    (autoload 'ec2-describe-volumes "aws")
    (autoload 'ec2-describe-snapshots "aws")
    (autoload 'ec2-describe-group "aws")
    (autoload 'ec2-get-console "aws")))
;; Block = @243258
(message "org-dotemacs: evaluating @243258 block")
  (use-package wakatime-mode
    :if (executable-find "wakatime")
    :hook
    (prog-mode . wakatime-mode)
    :init
    (defun wakatime-dashboard ()
      (interactive)
      (browse-url "https://wakatime.com/dashboard"))
    :custom
    (wakatime-cli-path
    "/usr/local/lib/python3.7/site-packages/wakatime/cli.py")
    (wakatime-python-bin
      "/usr/local/Cellar/python/3.7.2/bin/python3"))
;; Block = @243734
(message "org-dotemacs: evaluating @243734 block")
(use-package logview)
;; Block = @243819
(message "org-dotemacs: evaluating @243819 block")
(use-package lognav-mode)
;; Block = @243893
(message "org-dotemacs: evaluating @243893 block")
(use-package floobits
  :if (file-exists-p (user-home ".floorc.json")))

;; Block = @244013
(message "org-dotemacs: evaluating @244013 block")
(use-package rmsbolt)
;; Block = @244230
(message "org-dotemacs: evaluating @244230 block")
(use-package asm-mode
  :mode (("\\.64sa\\'" . asm-mode)
         ("\\.64da\\'" . asm-mode)
         ("\\.32sa\\'" . asm-mode)
         ("\\.32da\\'" . asm-mode)))
;; Block = @244461
(message "org-dotemacs: evaluating @244461 block")
(use-package nasm-mode
  :mode
  (("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode)))

;; Block = @244621
(message "org-dotemacs: evaluating @244621 block")
(use-package x86-lookup
  :init
  (when (featurep 'pdf-tools)
    (setq x86-lookup-browse-pdf-function
          'x86-lookup-browse-pdf-pdf-tools))

  (defun x86-lookup-install-pdf ()
    (interactive)
    (let ((install-directory (emacs-var-dir "x86-lookup"))
          (remote-url "https://software.intel.com/sites/default/files/managed/39/c5/325462-sdm-vol-1-2abcd-3abcd.pdf"))
      (unwind-protect
          (unless (file-directory-p install-directory)
            (make-directory install-directory)
            (cd install-directory)
            (url-retrieve
             remote-url
             (lambda (s)
               (write-region (point) (point-max) "manual.pdf"))))
        (setq x86-lookup-pdf
              (expand-file-name "manual.pdf" install-directory)))))

  :init
  (x86-lookup-install-pdf))

;; Block = @245510
(message "org-dotemacs: evaluating @245510 block")
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-top-down-recurring
                 "compile_commands.json"))

  (defun cmake-build-compilation-database ()
    (interactive)
    (let* ((project-directory (cond (projectile-project-root
                                     projectile-project-root)
                                    ((eq 'major-mode 'dired-mode)
                                     dired-directory)
                                    (t default-directory)))
           (build-directory (f-join project-directory "build"))
           (compiledb (f-join build-directory "compile_commands.json")))
      (when (f-exists? (f-join project-directory "CMakeLists.txt"))
        (if-not (f-exists? build-directory)
            (mkdir build-directory))
        (with-temp-buffer
          (shell-command "make clean")
          (cd build-directory)
          (cmake-command-run "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-directory))
        (if (f-exists? compiledb)
            (make-symbolic-link (f-slash project-directory) build-directory t)))))

  (defun cmake-mode-dash-docsets ()
    (setq-local dash-plugin-keywords '("cmake")))

  :hook (cmake-mode . cmake-mode-dash-docsets))

;; Block = @246927
(message "org-dotemacs: evaluating @246927 block")

(use-package cquery
    :commands lsp-cquery-enable
    :if (executable-find "cquery")
    :custom
    (cquery-executable (executable-find "cquery"))
    (cquery-extra-init-params '(:index (:comments 2)
                                :cacheFormat "msgpack"
                                :completion (:detailedLabel t)))
    :init
    (defun cquery/enable ()
      "Enable cquery in the workspace."
      (condition-case nil
          (lsp-cquery-enable)
        (user-error "%s" "cquery didn't work")))

    :hook
    ((c-mode c++-mode)  . cquery/enable)
    ((c-mode c++-mode) . halidom/flycheck-enable-hook)

    :config

    ;; Alternatively, use lsp-ui-peek interface
    ;; (setq cquery-sem-highlight-method 'font-lock)
    ;; alternatively
    (setq cquery-sem-highlight-method 'overlay)

    ;; For rainbow semantic highlighting
    (with-eval-after-load 'lsp-ui-mode
      (lsp-ui-peek-find-custom 'base "$cquery/base")
      (lsp-ui-peek-find-custom 'callers "$cquery/callers")
      (lsp-ui-peek-find-custom 'vars "$cquery/vars"))

    (cquery-use-default-rainbow-sem-highlight)

    (with-eval-after-load 'projectile
        (add-to-list 'projectile-project-root-files-top-down-recurring
                     ".cquery")))

;; Block = @248228
(message "org-dotemacs: evaluating @248228 block")
(use-package rtags
    :ensure-system-package (rdm . rtags)
    :hook
    ((c-mode c++-mode objc-mode) . rtags-start-process-unless-running))

;; Block = @248437
(message "org-dotemacs: evaluating @248437 block")
(use-package llvm-mode
    :straight nil
    :load-path "~/.emacs.d/etc/local/llvm-mode"
    :init
    (require 'llvm-mode)
    (require 'tablegen-mode))
;; Block = @248664
(message "org-dotemacs: evaluating @248664 block")
(use-package flycheck-clang-tidy
    :after flycheck
    :if (executable-find "clang-tidy")
    :init
    (defun clang-tidy/enable ()
      (when (and (-non-nil projectile-project-root)
                 (string= projectile-project-type "cmake"))
        (when (fboundp 'flycheck-mode)
          (flycheck-clang-tidy-setup))))
    (add-hook 'c-mode-common-hook 'clang-tidy/enable))

;; Block = @249123
(message "org-dotemacs: evaluating @249123 block")
(use-package clang-format
    :if (executable-find "clang-format")
    :bind (:map c-mode-base-map
                ("C-c i" . clang-format-region)
                ("C-c u" . clang-format-buffer))
    :custom
    (clang-format-style-option "google"))

;; Block = @249464
(message "org-dotemacs: evaluating @249464 block")
(use-package emacs-lisp-mode
  :straight nil
  :init
  (defun emacs-lisp/setup ()
    "Setup elisp."
    (when (require 'highlight-symbol nil t)
      (highlight-symbol-mode +1))
    (when (require 'rainbow-delimiters nil t)
      (rainbow-delimiters-mode +1))
    (setq lisp-indent-function 'lisp-indent-function)
    (setq-local dash-plugin-keywords '("elisp")))

  :hook
  (emacs-lisp-mode . emacs-lisp/setup))
;; Block = @249941
(message "org-dotemacs: evaluating @249941 block")
(use-package lisp-extra-font-lock
  :init
  (lisp-extra-font-lock-global-mode +1))
;; Block = @250098
(message "org-dotemacs: evaluating @250098 block")
  (use-package elisp-format
    :custom
    (elisp-format-column 80))
;; Block = @250279
(message "org-dotemacs: evaluating @250279 block")
(use-package lisp-mode
  :straight nil
  :init
  (defun lisp-setup ()
    "setup for common lisp."
    (if (fboundp 'highlight-symbol-mode)
        (highlight-symbol-mode +1))
    (if (eq system-type 'darwin)
        (setq-local dash-plugin-keywords '("lisp"))))
  :hook
  (lisp-mode . lisp-setup))
;; Block = @250741
(message "org-dotemacs: evaluating @250741 block")
(use-package slime
  :commands slime
  :defines
  (slime-complete-symbol*-fancy
   slime-completion-at-point-functions)
  :init
  (setq slime-contribs
        '(slime-fancy
          slime-indentation
          slime-references
          slime-tramp
          slime-scratch)
	      inferior-lisp-program "clisp"
      	;; enable fuzzy matching in code buffer and SLIME REPL
      	slime-complete-symbol*-fancy t
      	slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
  (defun slime/disable-smartparens ()
    "Disable smartparens in slime repl buffers."
    (when (fboundp 'smartparens-mode)
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode)))
  :hook
  (slime-repl-mode . slime/disable-smartparens))
;; Block = @251847
(message "org-dotemacs: evaluating @251847 block")
(use-package clojure-mode
  :custom (clojure-indent-style :always-indent)

  :mode (("\\.edn$"   . clojure-mode)
         ("\\.cljs$"  . clojurescript-mode)
         ("\\.cljx$"  . clojurex-mode)
         ("\\.cljsc$" . clojurec-mode))

  :hook ((clojure-mode . enable-paredit-mode)
         (clojure-mode . show-paren-mode))

  :config

  (defun halidom/clj-dash-docsets ()
    "Keywords for Clojure Docsets via Dash.app."
    (setq-local dash-plugin-keywords '("clojure")))

  (defun halidom/clj-style-compojure ()
    "Indendation for macros defined in compojure routing framework.

     See https://github.com/weavejester/compojure/wiki/Emacs-indentation."

    (define-clojure-indent
        (defroutes 'defun)
        (GET 2)
        (POST 2)
        (PUT 2)
        (DELETE 2)
        (HEAD 2)
        (ANY 2)
        (OPTIONS 2)
        (PATCH 2)
        (rfn 2)
        (let-routes 1)
        (context 2)))


  (defun halidom/clj-style-om-next ()
    "Indendation for om-next macros."
      (put-clojure-indent 'defui '(1 nil nil (1)))
      (put-clojure-indent 'dom/div 1))

  (defun halidom/clj-style-guide ()
    "Styleguide for clojure."
    (halidom/clj-style-compojure)
    (halidom/clj-style-om-next))

  (add-hook 'clojure-mode-hook #'halidom/clj-dash-docsets)
  (add-hook 'clojure-mode-hook #'halidom/clj-style-guide))
;; Block = @253339
(message "org-dotemacs: evaluating @253339 block")
(use-package clojure-mode-extra-font-locking
  :config
  (defun clj/enable-extra-font-locking ()
    (require 'clojure-mode-extra-font-locking))
  :hook
  (clojure-mode . clj/enable-extra-font-locking))
;; Block = @253801
(message "org-dotemacs: evaluating @253801 block")
(use-package cider
  :custom
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-result-prefix ";; => ")
  (cider-repl-wrap-history t)
  (cider-repl-history-size 3000)
  (cider-show-error-buffer nil)
  (nrepl-hide-special-buffers t)
  :hook
  (clojure-mode . cider-mode)
  ((cider-mode cider-repl-mode) . eldoc-mode)
  ((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
  (cider-repl-mode . subword-mode))
;; Block = @254306
(message "org-dotemacs: evaluating @254306 block")
(use-package clj-refactor
  :init
  (defun clj/refactor-enable ()
    "Enable clj-refactor in clojure-mode."
    (clj-refactor-mode 1)
    ;; For adding reuire/use/import statements
    (yas-minor-mode 1)
    ;; Unbinds `cider-macroexpand-1'
    (cljr-add-keybindings-with-prefix "C-c C-m"))

  :hook (clojure-mode . clj/refactor-enable))

;; Block = @254717
(message "org-dotemacs: evaluating @254717 block")
;; elein

;; Cljsbuild

;; Block = @254900
(message "org-dotemacs: evaluating @254900 block")
(use-package elein
  :if (executable-find "lein")
  :straight t)
;; Block = @255156
(message "org-dotemacs: evaluating @255156 block")
(use-package cljsbuild-mode
  :if (executable-find "lein")
  :hook ((clojure-mode clojurescript-mode) . cljsbuild-mode))
;; Block = @255323
(message "org-dotemacs: evaluating @255323 block")
(use-package groovy-mode
  :mode  "\\.gradle\\'"
  :init
  (defun groovy/setup ()
    "Setup `groovy-mode' buffers."
    (setq groovy-indent-offset 2
          tab-width 4
          indent-tabs-mode nil
          c-indent-comments-syntactically-p t))
  :hook
  (groovy-mode . groovy-setup))
;; Block = @255690
(message "org-dotemacs: evaluating @255690 block")
(use-package java-mode
  :straight nil
  :init
  (defun java/setup ()
    "Setup `java-mode' buffers."
    (c-set-offset 'arglist-close '0)
    (setq indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2)
    (setq-local dash-plugin-keywords '("java" "gradle" "groovy")))
  :hook
  (java-mode . java/setup))

;; Block = @256331
(message "org-dotemacs: evaluating @256331 block")
(use-package autodisass-java-bytecode)
;; Block = @256581
(message "org-dotemacs: evaluating @256581 block")
(use-package gradle-mode
  :if (executable-find "gradle")
  :hook
  (java-mode . gradle-mode))
;; Block = @256762
(message "org-dotemacs: evaluating @256762 block")
(use-package lsp-java
  :defines (lsp-java-enable)
  :custom
  (lsp-java-server-install-dir
   (file-name-as-directory
    (emacs-var-dir "eclipse.jdt.ls" "server")))
  (lsp-highlight-symbol-at-point nil)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-eldoc-render-all nil)
  (lsp-java-compilation-guess-arguments t))

;; Block = @257160
(message "org-dotemacs: evaluating @257160 block")
(use-package lsp-java-treemacs
  :straight lsp-java
  :after (treemacs))
;; Block = @257294
(message "org-dotemacs: evaluating @257294 block")
;; Dap Mode

;; Dap Java

;; Block = @257387
(message "org-dotemacs: evaluating @257387 block")
(use-package dap-java
  :straight dap-mode
  :after (lsp-java))
;; Block = @257523
(message "org-dotemacs: evaluating @257523 block")
(use-package javadoc-lookup
  :bind
  ("C-h j" . javadoc-lookup)
  :custom
  (javadoc-lookup-completing-read-function #'ivy-completing-read))
;; Block = @257742
(message "org-dotemacs: evaluating @257742 block")
(use-package js2-mode
  :custom
  (js-indent-level 2)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode))
  :interpreter "node"
  :hook (js2-mode . lsp))
;; Block = @257973
(message "org-dotemacs: evaluating @257973 block")
(use-package json-mode
  :mode "\\.json\\'"
  :init
  (defun json-mode-style ()
    "Styleguide for JSON Mode."
    (set (make-local-variable 'js-indent-level) 2))

    (defun json-mode-faces ()
    (let ((foreground (face-foreground 'font-lock-variable-name-face)))
      (face-remap-add-relative 'font-lock-keyword-face
                               `(:slant normal :foreground ,foreground))))
  (defun json-inhibit-message ()
      (set (make-local-variable 'inhibit-message) t))

  :hook
  (json-mode . json-mode-style)
  (json-mode . json-mode-faces)
  (json-mode . json-inhibit-message))

;; Block = @258666
(message "org-dotemacs: evaluating @258666 block")
(use-package add-node-modules-path
  :if (executable-find "node")
  :init
  (progn
    (add-hook 'js-mode-hook #'add-node-modules-path)))
;; Block = @258871
(message "org-dotemacs: evaluating @258871 block")
(use-package npm-mode
    :if (executable-find "npm")
    :init
    (el-patch-feature npm-mode)
    (el-patch-defun npm-mode--exec-process (cmd)
      "Execute a process running CMD."
      (message (concat "Running " cmd))
      (compile cmd (el-patch-add t)))

    (defun npm-mode-npm-install-global (dep)
      (interactive "sEnter package name: ")
      (npm-mode--exec-process (format "npm i -g %s" dep)))

    :config
    (npm-global-mode)

    :hook
    (js-mode . npm-mode))
;; Block = @259423
(message "org-dotemacs: evaluating @259423 block")
(use-package yarn-mode
    :if (executable-find "yarn"))
;; Block = @259542
(message "org-dotemacs: evaluating @259542 block")
(use-package nvm
  :if (executable-find "nvm"))
;; Block = @259663
(message "org-dotemacs: evaluating @259663 block")
  (use-package typescript-mode
    :custom
    (typescript-indent-level 2)
    :hook
    (typescript-mode . lsp)
    (typescript-mode . subword-mode))
;; Block = @259880
(message "org-dotemacs: evaluating @259880 block")
  (use-package tide
    :functions (tide/setup)
    :after (:all typescript-mode)
    :demand t
    :init
    (defun tide/setup ()
      "Enable Tide Mode for Typescript."
      (tide-setup)
      (when (fboundp 'flycheck-mode)
        ;; add typescript checkers
        (flycheck-add-mode 'typescript-tslint 'typescript-mode)
        ;; add javascript checkers
        (flycheck-add-next-checker 'javascript-eslint
                                   'javascript-tide
                                   'append)
        (setq flycheck-check-syntax-automatically '(save mode-enabled)))
      (eldoc-mode +1))


    :hook
    (before-save . tide-format-before-save)
    (typescript-mode . tide/setup)
    (typescript-mode . tide-hl-identifier-mode))
;; Block = @260713
(message "org-dotemacs: evaluating @260713 block")
(use-package rjsx-mode
  :mode "\\.jsx\\'")
;; Block = @260827
(message "org-dotemacs: evaluating @260827 block")
  (use-package flycheck-jest
    :after flycheck
    :init
    (flycheck-jest-setup))

;; Block = @260985
(message "org-dotemacs: evaluating @260985 block")
(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))
;; Block = @261115
(message "org-dotemacs: evaluating @261115 block")
(use-package indium
  :bind (:map indium-interaction-mode-map
                ("C-M-b" . indium-eval-buffer))
    :init
    (el-patch-feature indium-nodejs)
    (with-eval-after-load 'indium-nodejs
      (el-patch-defun indium-nodejs--process-filter-function (conf)
        "Return a process filter function for CONF.
The function detects the socket URL to connect to from the
process outputp."
        (let ((connected))
          (lambda (process output)
            ;; Append output to the process buffer
            (with-current-buffer (process-buffer process)
	            (goto-char (point-max))
	            (insert output)
              (el-patch-add
                (save-excursion
                  (goto-char (point-min))
                  (ansi-color-apply-on-region (point-min) (point-max)))))
            (when (and (not connected)
		                 (string-match-p "Debugger listening on" output))
	            ;; Node will keep outputing the "Debugger listening on" message after
	            ;; each deconnection, so only try to connect one.
	            (setq connected t)
	            (let-alist conf
	              (indium-client-connect (file-name-directory .projectFile) .name)))))))


    (cl-defun chrome-debugger-launch (&optional
                                        (port "3000")
                                        (host "localhost")
                                        (type "http"))
      "Launch a chromium debugger process on HOST using PORT and protocol TYPE.

      Note this will kill any running instances of Chromium."

      (interactive (list
                    (read-string "Port: " "3000")
                    (read-string "Host: " "localhost")
                    (read-string "Type: " "http")))

      (unless (featurep 'secrets)
        (require 'secrets))

      (if (boundp 'google-api-key)
          (setenv "GOOGLE_API_KEY" google-api-key))
      (if (boundp 'google-api-key)
          (setenv "GOOGLE_DEFAULT_CLIENT_ID" google-default-client-id))
      (if (boundp 'google-default-client-secret)
          (setenv "GOOGLE_DEFAULT_CLIENT_SECRET" google-default-client-secret))
      (let ((process-names (--> (process-list)
                                (mapcar (lambda (p) (process-name p)) it)))
            (process (get-process "chromium"))
            (ip (concat type "://" host ":" port))
            (chromium-program browse-url-chromium-program))
        (if (shell-command-to-list "pgrep \"Chromium\"")
            (shell-command "killall Chromium"))
        (if (process-live-p process)
            (kill-process process))
        (start-process "chromium" (get-buffer-create "chromium")
                       chromium-program "--remote-debugging-port=9222" ip)))
    :hook
    (js-mode . indium-interaction-mode))
;; Block = @263987
(message "org-dotemacs: evaluating @263987 block")
(use-package python-mode
  :init
  (when (executable-find "ipython3")
    (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "-i"))
  :config
  (defun python-dash-docsets ()
    (setq-local dash-plugin-keywords
                '("python" "django" "twisted" "sphinx"
                  "flask" "tornado" "sqlalchemy" "numpy"
                  "scipy" "salt" "pandas" "matplotlib"
                  "cvp")))
  (when (eq system-type 'darwin)
    (add-hook 'python-mode-hook #'python-dash-docsets))
  :hook
  (python-mode . lsp))
;; Block = @264667
(message "org-dotemacs: evaluating @264667 block")
  (use-package pyenv-mode
    :if (executable-find "pyenv")
    :init
    (add-to-list 'exec-path (user-home ".pyenv" "shims"))
    (setenv "WORKON_HOME" (file-name-as-directory (user-home ".pyenv" "versions")))
    (pyenv-mode)
    :bind
    ("C-x p e" . pyenv-activate-current-project)
    ("C-x p s" . pyenv-mode-set)
    :commands

    (pyenv-mode-set pyenv-mode-unset pyenv-mode-versions)

    :config

    (defvar pyenv-current-version nil nil)

    (defun pyenv-init()
      "Initialize pyenv's current version to the global one."
      (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
        (message (concat "Setting pyenv version to " global-pyenv))
        (pyenv-mode-set global-pyenv)
        (setq pyenv-current-version global-pyenv)))

    (add-hook 'after-init-hook 'pyenv-init)

    (progn
      (with-eval-after-load 'projectile
        (defun projectile-pyenv-mode-set ()
          "Set pyenv version matching project name."
          (let ((project (projectile-project-name)))
            (if (member project (pyenv-mode-versions))
                (pyenv-mode-set project)
              (pyenv-mode-unset))))

        (add-hook 'projectile-after-switch-project-hook
                  'projectile-pyenv-mode-set))

      ;; http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
      (defun pyenv-activate-current-project ()
        "Automatically activates pyenv version if .python-version file exists."
        (interactive)
        (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
          (if python-version-directory
              (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
                     (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
                (pyenv-mode-set pyenv-current-version)
                (message (concat "Setting virtualenv to " pyenv-current-version))))))))
;; Block = @266715
(message "org-dotemacs: evaluating @266715 block")
(use-package pyenv-mode-auto)
;; Block = @266833
(message "org-dotemacs: evaluating @266833 block")
(use-package virtualenvwrapper
    :init
    (setq venv-dirlookup-names '(".pyenv" ".venv"))
    (setq venv-location (getenv "WORKON_HOME"))
    :config
    (when (fboundp 'projectile-mode)
      (setq projectile-switch-project-action
            '(lambda ()
              (venv-projectile-auto-workon)
              (projectile-find-file)))))

;; Block = @267241
(message "org-dotemacs: evaluating @267241 block")
(use-package pyvenv
    :requires virtualenvwrapper
    :init
  (pyvenv-mode +1))
;; Block = @267373
(message "org-dotemacs: evaluating @267373 block")
(use-package with-venv)
;; Block = @267449
(message "org-dotemacs: evaluating @267449 block")
(use-package live-py-mode)
;; Block = @267548
(message "org-dotemacs: evaluating @267548 block")
(use-package pip-requirements
    :straight t)
;; Block = @267656
(message "org-dotemacs: evaluating @267656 block")
(use-package pydoc
    :straight t)
;; Block = @267848
(message "org-dotemacs: evaluating @267848 block")
(use-package ein
    :config
    (setq ein:use-smartrep t))
;; Block = @267974
(message "org-dotemacs: evaluating @267974 block")
(use-package prolog-mode
  :straight nil
  :mode "\\.pl\\'")
;; Block = @268112
(message "org-dotemacs: evaluating @268112 block")
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config

  (defun halidom/ruby-dash-docsets ()
    (setq-local dash-plugin-keywords '("ruby" "rails")))


  (add-hook 'ruby-mode-hook #'halidom/ruby-dash-docsets))

;; Block = @268424
(message "org-dotemacs: evaluating @268424 block")
(use-package enh-ruby-mode
    :after ruby-mode
    :demand t
    :mode "\\.rb\\'"
    :config
    (add-hook 'enh-ruby-mode-hook #'halidom/ruby-dash-docsets))
;; Block = @268649
(message "org-dotemacs: evaluating @268649 block")
(use-package inf-ruby
    :hook
    ((ruby-mode enh-ruby-mode) . inf-ruby-minor-mode)
    (compilation-filter . inf-ruby-auto-enter))
;; Block = @268844
(message "org-dotemacs: evaluating @268844 block")
(use-package rvm
  :init
  (rvm-use-default))
;; Block = @268953
(message "org-dotemacs: evaluating @268953 block")
  (use-package robe
    :init
    ;; ensure `rvm' activates the proper project Ruby
    ;; before `robe-start' runs.
    (when (featurep 'rvm)
      (defadvice inf-ruby-console-auto
          (before activate-rvm-for-robe activate)
        (rvm-activate-corresponding-ruby)))

    (defun robe/enable ()
      "Enable `robe-mode'."
      (robe-mode 1)
      (when (fboundp 'company-mode)
        (let ((backends (cons 'company-robe company-backends)))
          (set (make-local-variable 'company-backends) backends))))

    :hook
    ((ruby-mode enh-ruby-mode) . robe/enable))
;; Block = @269598
(message "org-dotemacs: evaluating @269598 block")
(use-package yard-mode
  :hook
  ((ruby-mode enh-ruby-mode) . yard-mode)
  ((ruby-mode enh-ruby-mode) . eldoc-mode))

;; Block = @269790
(message "org-dotemacs: evaluating @269790 block")
(use-package utop)
;; Block = @269877
(message "org-dotemacs: evaluating @269877 block")
(use-package merlin
  :custom
  (merlin-command 'opam)
  (merlin-error-after-save nil))
;; Block = @270034
(message "org-dotemacs: evaluating @270034 block")
(use-package caml
  :hook
  (caml . ocaml/merlin))
;; Block = @270153
(message "org-dotemacs: evaluating @270153 block")
  (use-package tuareg
    :mode
    ("\\.ml\\'" . tuareg-mode)

    :init
    (defun tuareg/prettify-symbols ()
      "Enable `prettify-symbols-mode' for `tuareg-mode'."
      (when (functionp 'prettify-symbols-mode)
        (prettify-symbols-mode)))

    :hook
    (tuareg-mode . tuareg/prettify-symbols)
    (tuareg-mode . merlin-mode))
;; Block = @270556
(message "org-dotemacs: evaluating @270556 block")
(use-package dune
  :straight
  (dune
   :host github
   :type git
   :repo "ocaml/dune"
   :files ("editor-integration/emacs/dune"
           "editor-integration/emacs/dune.el"
           "editor-integration/emacs/dune-flymake.el"))
  :mode
  ("dune\\'" . dune-mode)
  ("dune-project\\'" . dune-mode)

  :ensure-system-package
  (dune . "opam install dune")
  :init
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'dune
                                      '("dune-project")
                                      :compile "dune build"
                                      :test "dune runtest"
                                      :run "dune exec"))
  :hook
  (dune-mode . enable-paredit-mode))
;; Block = @271353
(message "org-dotemacs: evaluating @271353 block")
(use-package reason-mode
  :hook
  (reason . merlin-mode))
;; Block = @271492
(message "org-dotemacs: evaluating @271492 block")
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))
;; Block = @271619
(message "org-dotemacs: evaluating @271619 block")
(use-package sbt-mode)
;; Block = @271699
(message "org-dotemacs: evaluating @271699 block")
(use-package sh-mode
  :straight nil
  :custom
  (sh-indentation 2)
  :init
  (defun sh-mode/disable-org-link ()
    (org-link-minor-mode -1))
  :hook
  (sh-mode  . sh-mode/disable-org-link))

;; Block = @271949
(message "org-dotemacs: evaluating @271949 block")
  (use-package web-mode
    :bind
    (:map web-mode-map
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
     ("\\.djhtml\\'"     . web-mode)
     ("\\.tsx\\'"        . web-mode))

    :custom
    (web-mode-engines-alist
     '(("php" . "\\.phtml\\'")
       ("blade" . "\\.blade\\'")))
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-css-colorization t)
    (web-mode-enable-block-face t)
    (web-mode-enable-part-face t)
    (web-mode-enable-comment-keywords t)
    (web-mode-enable-heredoc-fontification t)
    (web-mode-enable-current-element-highlight t)
    (web-mode-enable-current-column-highlight t)
    (web-mode-markup-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-style-padding 1)
    (web-mode-script-padding 1)
    (web-mode-block-padding 0)
    (web-mode-comment-style 2)

    :config

    (defun web/tsx ()
      "Enable tide-mode in Typescript tsx buffers."
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (when (fboundp 'tide/setup)
          (tide/setup))
        (when (fboundp 'flycheck-add-mode)
          (flycheck-add-mode 'typescript-tslint 'web-mode))))

    (defun web/docsets ()
      "Set Dash docsets for `web-mode'"
      (when (eq system-type 'darwin)
        (setq-local dash-plugin-keywords
                    '("css" "html" "javascript" "react"))))
    :hook
    (web-mode . web/tsx)
    (web-mode . web/docsets))
;; Block = @273963
(message "org-dotemacs: evaluating @273963 block")
(use-package html-mode
  :straight nil
  :hook
  (html-mode . lsp))

;; Block = @274103
(message "org-dotemacs: evaluating @274103 block")
  (use-package tagedit
    :init
    (defun tagedit/enable ()
      "Enable `tagedit-mode'"
      (tagedit-add-experimental-features)
      (tagedit-mode +1))

    :hook
    (html-mode . tagedit/enable))
;; Block = @274364
(message "org-dotemacs: evaluating @274364 block")
(use-package auto-rename-tag)
;; Block = @274463
(message "org-dotemacs: evaluating @274463 block")
(use-package htmlize)
;; Block = @274561
(message "org-dotemacs: evaluating @274561 block")
(use-package css-mode
  :custom (css-indent-offset 2)
  :hook
  (css-mode . lsp))
;; Block = @274706
(message "org-dotemacs: evaluating @274706 block")
(use-package less-mode
  :mode "\\.less\\'"
  :hook
  (less-mode . lsp))
;; Block = @274847
(message "org-dotemacs: evaluating @274847 block")
(use-package sass-mode
  :mode "\\.sass\\'"
  :hook
  (sass-mode . lsp))
;; Block = @274988
(message "org-dotemacs: evaluating @274988 block")
(use-package scss-mode
  :mode "\\.scss\\'"
  :hook
  (scss-mode . lsp))
;; Block = @275125
(message "org-dotemacs: evaluating @275125 block")
(use-package nginx-mode
    :mode ("/nginx/sites-\\(?:available|enabled\\)/" . nginx-mode))
;; Block = @275281
(message "org-dotemacs: evaluating @275281 block")
(use-package emmet-mode
  :hook
  ((css-mode html-mode web-mode) . emmet-mode))
;; Block = @275496
(message "org-dotemacs: evaluating @275496 block")
  (use-package markdown-mode
    :preface
    (defun markdown-open-preview ()
      "Use Marked 2 to preview the current file"
      (interactive)
      (let ((app-list (mapcar #'car (counsel-osx-app-list))))
        (if (member "Marked" app-list)
            (shell-command
             (format
              "open -a 'Marked' %s"
              (shell-quote-argument (buffer-file-name)))))))

      :custom
      (markdown-command "multimarkdown")
      (markdown-open-command #'markdown-open-preview)
      :mode
      (("README\\.md\\'" . gfm-mode)
       ("\\.md\\'" . markdown-mode)
       ("\\.markdown\\'" . markdown-mode))
      :hook
      ((markdown-mode gfm-mode) . visual-fill-column-mode))
;; Block = @276270
(message "org-dotemacs: evaluating @276270 block")
(use-package markdown-mode+)
;; Block = @276367
(message "org-dotemacs: evaluating @276367 block")
(use-package markdown-toc)
;; Block = @276461
(message "org-dotemacs: evaluating @276461 block")
(use-package markdownfmt
  :bind (:map markdown-mode-map
	      ("C-c C-f" . markdown-format-buffer))
  :hook
  (markdown-mode . markdownfmt-enable-on-save))
;; Block = @276688
(message "org-dotemacs: evaluating @276688 block")
  (use-package livedown
      :straight (livedown
                 :type git
                 :host github
                 :repo "shime/emacs-livedown")
      :custom (livedown-open nil)
      :init

      (when (featurep 'xwidget-internal)
        (unless (featurep 'xwidget)
          (require 'xwidget))

        (defadvice livedown-preview (after livedown-preview-after activate)
          (xwidget-webkit-browse-url "http://localhost:1337"))))
;; Block = @277190
(message "org-dotemacs: evaluating @277190 block")
(use-package apples-mode
  :mode "\\.applescript\\'")
;; Block = @277310
(message "org-dotemacs: evaluating @277310 block")
(use-package racket-mode
  :hook
  (racket . paredit-mode))
;; Block = @277432
(message "org-dotemacs: evaluating @277432 block")

(use-package yaml-mode
    :mode (("\\.yml\\'" . yaml-mode)
           (".clang-tidy\\'" . yaml-mode)))

(message "org-dotemacs: 563 blocks evaluated.")
(message "org-dotemacs: 0 blocks not considered (see ~/.emacs.d/dotemacs.org).")
