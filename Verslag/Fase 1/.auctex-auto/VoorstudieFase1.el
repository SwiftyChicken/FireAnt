(TeX-add-style-hook
 "VoorstudieFase1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("report" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("babel" "dutch") ("xcolor" "table" "xcdraw")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "chapters/1_Introductie"
    "chapters/2_ADTs"
    "chapters/3_Afhankelijkheidsdiagram"
    "chapters/4_Planning"
    "report"
    "rep12"
    "inputenc"
    "babel"
    "graphicx"
    "titlesec"
    "etoolbox"
    "parskip"
    "hyperref"
    "amssymb"
    "xcolor"
    "tikz"))
 :latex)

