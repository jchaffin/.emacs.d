(TeX-add-style-hook
 "uclacs-preview"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("algorithm2e" "algoruled" "linesnumbered")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "fontspec"
    "amsmath"
    "unicode-math"
    "subcaption"
    "algorithm2e"
    "varioref"
    "listings")
   (TeX-add-symbols
    '("forcondj" 2)
    '("forcondi" 2)
    '("forcond" 3)
    "nosemic"
    "dosemic"
    "pushline"
    "popline"))
 :latex)

