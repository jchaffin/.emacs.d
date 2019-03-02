;;; -*- lexical-binding: t ; -*-
;;; early-init.el -- Emacs early initialization file
;;
;; Copyright (c) 2018 Jacob Chaffin
;; Author Jacob Chaffin
;;
;; Homepage: https://github.com/.emacs.d.git
;;
;; Commentary:
;;
;; From the Emacs 27.1 NEWS:
;;
;;
;;    Emacs can now be configured using an early init file.
;;    The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;;    loaded very early in the startup process: before graphical elements
;;    such as the tool bar are initialized, and before the package manager
;;    is initialized.  The primary purpose is to allow customizing how the
;;    package system is initialized given that initialization now happens
;;    before loading the regular init file (see below).
;;    PKG_COMMENTARY
;;
;;
;;; Code:

(setq package-enable-at-startup nil)


;;; early-init.el ends here
