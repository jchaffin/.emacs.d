(TeX-add-style-hook
 "uclasem-preview"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("forest" "linguistics" "edges")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "amsmath"
    "stmaryrd"
    "fontspec"
    "bussproofs"
    "unicode-math"
    "cabin"
    "xpatch"
    "gb4e"
    "adjustbox"
    "tikz"
    "forest")
   (TeX-add-symbols
    '("objlang" ["argument"] 1)
    '("abar" 0)
    '("Agree" 0)
    '("startfn" 0)
    '("denotationbase" 1)
    '("denotation" 1)
    '("xbar" 1)
    '("comp" 1)
    '("spec" 1)
    '("head" 1)
    '("sagrf" 1)
    '("smf" 1)
    '("shf" 1)
    '("agrf" 1)
    '("hf" 1)
    '("mf" 1)
    '("feature" 1)
    '("ind" 1)
    '("gap" 0)
    '("ja" 0)
    '("littlev" 0)
    '("vP" 0)
    '("tns" 1)
    '("nnagree" 1)
    '("nagree" 1)
    '("ppagree" 1)
    '("pagree" 1)
    '("plusfs" 1)
    '("selfs" 1)
    '("plusfc" 1)
    '("selfc" 1)
    '("plusf" 1)
    '("self" 1)
    '("ml" 1)
    '("tkn" 2)
    "boldcheckmark"
    "nl"
    "vb")
   (LaTeX-add-environments
    "tikzbracket")
   (LaTeX-add-lengths
    "ArrowHeight"
    "BelowArrowSkip"))
 :latex)

