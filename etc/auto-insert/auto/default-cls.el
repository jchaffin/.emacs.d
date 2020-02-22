(TeX-add-style-hook
 "default-cls"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (LaTeX-add-environments
    '("problem" LaTeX-env-args ["argument"] 0)))
 :latex)

