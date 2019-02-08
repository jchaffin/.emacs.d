(TeX-add-style-hook
 "uclacs-preview"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "fontspec"
    "amsmath"
    "unicode-math"
    "hologo"
    "tikz"
    "forest")
   (TeX-add-symbols
    "vertex")
   (LaTeX-add-environments
    '("problem" LaTeX-env-args ["argument"] 0)))
 :latex)

