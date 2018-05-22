if (exists('g:c_operator_hightlight')) && g:c_operator_hightlight
    syn match cppDelimiter "[~,;:(){}\[\]]"
    syn match cppPunctOp "[+\-<>=!|&^?]\|\%(/\@<!\*\)\|\%([/*]\@<!/[/*]\@!\)"
endif

hi link cppDelimiter       Delimiter
hi link cppPunctOp         Operator
