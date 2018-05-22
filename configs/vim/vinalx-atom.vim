" The theme is based on
" [atom](http://www.gregsexton.org/vim-color-schemes/atom-color/)
" by Greg Sexton
"
" Refined and maintained by [VinaLx](https://github.com/VinaLx/)

set background=dark
if version > 580
    "no guarantees for version 5.8 and below, but this makes it stop complaining
    hi clear
    if exists("syntax_on")
	syntax reset
    endif
endif
let g:colors_name="vinalx-atom"

hi Normal       guifg=#CCCCCC     guibg=#273240

hi DiffDelete   guifg=#304050     guibg=#203040
hi DiffAdd      guibg=#002851
hi DiffChange   guibg=#450303
hi DiffText     guibg=#990909     gui=NONE

hi diffAdded    guifg=#00BF00     guibg=#1d2c1b
hi diffRemoved  guifg=#E00000     guibg=#2d1c20

hi Cursor       guifg=#778899     guibg=#F0E687       
hi VertSplit    guibg=#102030     guifg=#102030   gui=NONE
hi Folded       guifg=#CCCCCC     guibg=#405060
hi FoldColumn   guibg=grey30      guifg=tan
hi IncSearch    guifg=#778899   guibg=#F0E687
hi LineNr       guifg=#556575     guibg=#203040
hi ModeMsg      guifg=goldenrod
hi MoreMsg      guifg=SeaGreen
hi NonText      guifg=#304050     guibg=#273240
hi Question     guifg=springgreen
hi Search       guibg=#FFFF7D     guifg=#000000
hi SpecialKey   guifg=yellowgreen
hi StatusLine   guibg=#102030     guifg=grey70    gui=NONE
hi StatusLineNC guibg=#203040     guifg=grey50    gui=NONE
hi Title        guifg=#CD5C5C
hi Visual       gui=NONE          guifg=NONE     guibg=#506072
hi WarningMsg   guifg=salmon
hi Directory    guifg=#70D080
hi SignColumn   guifg=#A6E22E     guibg=#203040

hi SpellBad     guisp=#CD5C5C     gui=undercurl
hi SpellCap     guisp=#ECAD2B     gui=undercurl

if version >= 700 " Vim 7.x specific colorl
    hi CursorLine   guifg=NONE    guibg=#384050 gui=NONE
    hi CursorColumn guifg=NONE    guibg=#384050 gui=NONE
    hi MatchParen   guifg=#EFE580 guibg=#7D8D9E gui=BOLD
    hi Pmenu        guifg=#EAEBED guibg=#315E81 gui=NONE
    hi PmenuSel     guifg=#30404E guibg=#B2D7FF gui=NONE
endif

if version >= 703 " Vim 7.x specific colors
    hi ColorColumn  guifg=NONE guibg=#353A53
endif

" syntax highlighting groups
hi Comment    guifg=#8090A0   gui=NONE
hi Constant   guifg=#BB55BB   gui=NONE
hi Keyword    guifg=#8D8DAE   gui=NONE
hi String     guifg=#ECBE70   gui=NONE
hi Identifier guifg=#C1DFFF   gui=NONE
hi Statement  guifg=#84ADD9   gui=NONE
hi PreProc    guifg=#F16372   gui=NONE
hi Operator   guifg=#F16372   gui=NONE
hi Function   guifg=#86BF6B   gui=NONE
hi Type       guifg=#19B9C4   gui=NONE
hi Special    guifg=#ECAD2B   gui=NONE
hi Delimiter  guifg=#8C9BA9
hi Number     guifg=#D697DB
hi Ignore     guifg=grey40    gui=NONE
hi Todo       guifg=orangered guibg=#304050 gui=NONE
hi link Boolean   Constant
hi link Character Constant

"vim: sw=4
