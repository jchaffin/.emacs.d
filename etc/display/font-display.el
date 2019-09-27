;;; font-display.el ---  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019, Jacob Chaffin, all rights reserved.

;; Version: 0.0.1
;; Author: Jacob Chaffin -- <jchaffin@ucla.edu>
;; URL: https://github.com/jchaffin/display
;; Created: 13 September 2019
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

(pretty-fonts-set-fontsets
  '(
    ("all-the-icons"
     ;;    
     69655 59688)
    ("Apple Color Emoji"
     ; ️📓   📒️     📝    📘       📚
     128213 128210 128221 128216 128218
     ;; 🎓   🗣      💻   📎
     127891 128483 128187 128206
     ;; ❌   🚫     🔗    🛒    🔈
     10060 128683 128279 128722 128264)
    ("file-icons"
     ;;                  
     58880 58881 59686 59671 59674)

    ("FontAwesome"
     ;;                     
     61856 61564 61641 61643 61463 61697)

    ("Material Icons"
     ;;                
     57439 58320 58321 58322 58324
     ;;                 
     58343 58389 58842 59505 59510
     ;; 
     59672)

    ("github-octicons"
     ;;                           
     61585 61529 61558 61557 61462 61553 61636 )))




(provide 'font-display)
;;; font-display.el ends here
