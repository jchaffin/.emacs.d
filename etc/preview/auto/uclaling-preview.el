(TeX-add-style-hook
 "uclaling-preview"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("mathtools" "fleqn" "tbtags") ("unicode-math" "math-style=literal") ("forest" "linguistics" "edges")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "mathtools"
    "fontspec"
    "unicode-math"
    "fixmath"
    "soul"
    "marvosym"
    "bussproofs"
    "gb4e"
    "adjustbox"
    "tikz"
    "forest"
    "arcs"
    "vowel"
    "stoneipa")
   (TeX-add-symbols
    '("objlang" ["argument"] 1)
    '("sem" ["argument"] 1)
    '("abar" 0)
    '("Agree" 0)
    '("xbar" 1)
    '("comp" 1)
    '("spec" 1)
    '("head" 1)
    '("littlev" 0)
    '("vP" 0)
    '("tns" 1)
    '("sagrf" 1)
    '("smf" 1)
    '("shf" 1)
    '("nnagree" 1)
    '("nagree" 1)
    '("ppagree" 1)
    '("pagree" 1)
    '("plusfs" 1)
    '("plusfc" 1)
    '("plusf" 1)
    '("selfc" 1)
    '("selfs" 1)
    '("self" 1)
    '("agrf" 1)
    '("hf" 1)
    '("mf" 1)
    '("feature" 1)
    '("ind" 1)
    '("gap" 0)
    '("ja" 0)
    '("arc" 2)
    '("vb" 0)
    '("cat" 1)
    '("Rangle" 0)
    '("Langle" 0)
    '("startfn" 0)
    '("denotationbase" 1)
    '("denotation" 1)
    '("ipa" 1)
    '("tkn" 2)
    '("Rule" 1)
    "boldcheckmark"
    "extraVskip")
   (LaTeX-add-environments
    '("problem" LaTeX-env-args ["argument"] 0)
    '("sprooftree" 1)
    "tikzbracket")
   (LaTeX-add-lengths
    "ArrowHeight"
    "BelowArrowSkip"))
 :latex)

