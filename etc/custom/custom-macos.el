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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/var/bookmark-default.el")
 '(load-prefer-newer t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))
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
