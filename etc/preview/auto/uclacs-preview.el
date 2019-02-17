(TeX-add-style-hook
 "uclacs-preview"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("algorithm2e" "algoruled" "linesnumbered")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "fontspec"
    "unicode-math"
    "cabin"
    "soul"
    "marvosym"
    "tikz"
    "forest"
    "clrscode3e"
    "algorithm2e")
   (TeX-add-symbols
    '("forcondj" 2)
    '("forcondi" 2)
    '("forcond" 3)
    "vertex"
    "nosemic"
    "dosemic"
    "pushline"
    "popline"))
 :latex)

