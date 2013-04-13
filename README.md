
Converts raw utf8 strings to latex [braillebox](ftp://ftp.tex.ac.uk/ctan/pub/tex/macros/latex/contrib/braille/braille.html) markup.

Usage(at ghci prompt)

*Main> utf8ToBrailleBox "Hello ⠁"
"Hello \\braillebox{1       }"

