" 基本设置 {{{
" be iMproved, required
set nocompatible
syntax enable
syntax on
filetype plugin on
filetype indent on
set clipboard=unnamed
set noswapfile
set nofoldenable
" set updatetime=250

" 设置alt键不映射到菜单栏
set winaltkeys=no

" 设置Vim文件的折叠方式为marker {{{
augroup ft_vim
    au!
au FileType vim setlocal foldmethod=marker
augroup END
" }}}

"输入法设置 {{{2
if has('multi_byte_ime')
"未开启IME时光标背景色
hi Cursor guifg=bg guibg=Orange gui=NONE
"开启IME时光标背景色
hi CursorIM guifg=NONE guibg=Skyblue gui=NONE
" 关闭Vim的自动切换IME输入法(插入模式和检索模式)
set iminsert=0 imsearch=0
" 插入模式输入法状态未被记录时，默认关闭IME
"inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
endif
"}}}

" 修改Leader Key {{{2
let mapleader=","
let localleader =","
let maplocalleader = ","
" }}}


" 设置Vim的GUI样式 {{{2
set guioptions-=m "去掉Menu Bar
set guioptions-=T "去掉工具栏
set guioptions-=r "去掉右边滚动栏
set guioptions-=L "去掉左边滚动栏
" }}}

" Windows下的编码设置 {{{2
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,cp936,gb18030,big5,euc-jp,euc-kr,latin1
set fileencoding=utf-8
" set termencoding=utf-8
"解决consle输出乱码
" language messages zh_CN.utf-8
" }}}

" 始终显示文件格式 {{{2
if has ('gui_running')
    let do_syntax_sel_menu = 1
endif
" }}}

" 设置自动补全时列出所有选项 {{{2
set wildmenu
set wildmode=list,full
" }}}

" 关闭VIM错误提示音 {{{2
set noeb
set vb t_vb=
" }}}

" Vim 启动后最大化窗口 {{{2
au GUIEnter * simalt ~x
" }}}

" 启动英文单位补全 {{{2
set dictionary+=$VIMRUNTIME/dict/english.dict
" }}}

" 文件在Vim之外修改过，自动重新读入 {{{2
set autoread
" }}}

" 搜索相关 {{{2
set hlsearch                " 搜索时，高亮显示查找内容
set incsearch               " 使用即时搜索
set ignorecase              " 搜索是，忽略大小写
set smartcase               " 如果搜索中有大写字符，忽略 'ignorecase' 选项"
" 去掉搜索高亮
noremap <silent><leader>/ :nohls<CR>
noremap n nzz
noremap N Nzz
" }}}

" 相对行号: 行号变成相对，可以用 nj/nk 进行跳转 {{{2
set number
function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber number
  else
    set relativenumber
  endif
endfunc
nnoremap <F4> :call NumberToggle()<cr>
" }}}

" Tab和空格 {{{2
set tabstop=4         " 设置Tab键的宽度=等同4个空格
set shiftwidth=4      " 每一级自动缩进的空格数为4个
set expandtab         " 将Tab自动转化成空格(需要输入真正的Tab键时，使用 Ctrl+V+Tab)
set softtabstop=4     " 按退格键时可以一次删掉 4 个空格  
" }}}

" 自动折行 {{{2
set textwidth=80
set formatoptions+=mM
set colorcolumn=81
" }}}

" 显示其他空白字符 {{{2
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<
set list
" }}}

" Key Mappings {{{2
" 交换分号与冒号位置 {{{3
nnoremap ; :
nnoremap : ;
" }}}
" Key mappings to ease browsing long lines {{{3
noremap  j         gj
noremap  k         gk
noremap  gj        j
noremap  gk        k
inoremap <M-Home> <C-O>g0
inoremap <M-End>  <C-O>g$
" }}}}
" 在按下<Leader>vi*后自动在新的tab里打开Vimrc {{{3
nnoremap <Leader>vim :tabe $MYVIMRC<cr>
nnoremap <Leader>viv :vsp $MYVIMRC<cr>
nnoremap <Leader>vis :sp $MYVIMRC<cr>
" }}}
" 使用<Leader>bd来彻底关闭buffer {{{3
nnoremap <Leader>bd :bdelete<cr>
" }}}
" 使用<Leader>gs来打开Git Status {{{3
nnoremap <Leader>gs :Gstatus<cr>
" }}}
" 移动分割窗口 {{{3
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
" " }}}
" 正常模式下 alt+j,k,h,l 调整分割窗口大小 {{{3
nnoremap <M-j> :resize +5<cr>
nnoremap <M-k> :resize -5<cr>
nnoremap <M-h> :vertical resize +5<cr>
nnoremap <M-l> :vertical resize -5<cr>
" }}}
" 插入模式移动光标 alt + 方向键 {{{3
inoremap <M-j> <Down>
inoremap <M-k> <Up>
inoremap <M-h> <left>
inoremap <M-l> <Right>
" }}}
" 命令模式下的行首尾 {{{3
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <home>
cnoremap <C-e> <end>
" }}}
" Go to home and end using capitalized directions {{{
noremap H ^
noremap L $
" }}}
" Highlight Word {{{
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
endfunction " }}}
" Mappings {{{
nnoremap <silent> <leader>0 :call clearmatches()<cr>
nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

" }}}
" Default Highlights {{{

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
" 4. Pulse the cursor line.  My eyes are bad.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
nnoremap <c-z> mzzMzvzz15<c-e>`z

" }}}

" }}}
" 可视模式下直接搜索已选择的区域 {{{
vnoremap // y/<C-R>"<CR>
" }}}
" }}}

" }}}

" 安装的插件 {{{
"NeoBundle设置
if 0 | endif
" set the runtime path to include Vundle and initialize
set rtp+=$HOME/vimfiles/bundle/neobundle.vim/
call neobundle#begin(expand('$HOME/vimfiles/bundle/'))
" let NeoBundle manage NeoBundle, required
NeoBundleFetch 'Shougo/neobundle.vim'

" 自定义插件
" 主题
NeoBundle 'crusoexia/vim-monokai'
" 语法检查
" NeoBundle 'scrooloose/syntastic'
" 状态栏
NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
" 优化折叠
" NeoBundle 'konfekt/fastfold'
" 自动补全工具
NeoBundle 'SirVer/ultisnips'
NeoBundle 'Shougo/neocomplete'
NeoBundle "honza/vim-snippets"
" 自动更改配对括号插件
NeoBundle 'tpope/vim-surround'
" LaTeX插件
NeoBundle 'lervag/vimtex'
" 使用自动对齐插件
NeoBundle 'junegunn/vim-easy-align'
" 自动插入comment的插件
NeoBundle 'tpope/vim-commentary'
" Rainbow Parentheses Improved
NeoBundle 'luochen1990/rainbow'
" easymotion
NeoBundle 'easymotion/vim-easymotion'
" Vim-Pandoc
" NeoBundle 'vim-pandoc/vim-pandoc'
" NeoBundle 'vim-pandoc/vim-pandoc-syntax'
" NeoBundle 'vim-pandoc/vim-pandoc-after'
" RMarkdown
" NeoBundle 'vim-pandoc/vim-rmarkdown'
" Nvim-R一个与 vim 集成的R开发环境
" NeoBundle 'jalvesaq/Nvim-R'
" vim 的表格环境
" NeoBundle 'dhruvasagar/vim-table-mode'
" Vim-Repeat
NeoBundle 'tpope/vim-repeat'
" Tabular表格对齐
NeoBundle 'godlygeek/tabular'
" Vim-Shell
" NeoBundle 'xolox/vim-shell'
" NeoBundle 'xolox/vim-misc'
" NeoBundle 'xolox/vim-session'
" Vim-Proc and Unite plugins
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neoyank.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'sgur/unite-everything'
NeoBundle 'Shougo/unite-help'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/unite-session'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'osyo-manga/unite-quickfix'
" Vim with Git
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
" Using VimFiler with typing and autocompletion
NeoBundle 'romgrk/vimfiler-prompt'
" Use more icons in Vim
NeoBundle 'ryanoasis/vim-devicons'
" Show marks visually in Vim
NeoBundle 'kshenoy/vim-signature'
" csv plugin
" NeoBundle 'chrisbra/csv.vim'
" Citation
" NeoBundle 'rafaqz/citation.vim'
" Asyncrun tasks
" NeoBundle 'skywind3000/asyncrun.vim'
" " Vim Quick Run
" NeoBundle 'thinca/vim-quickrun'
call neobundle#end()            " required
filetype plugin indent on    " required
" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
" }}}

" 插件设置 {{{
" " ywvim输入法设置 {{{
" let g:ywvim_ims=[
"             \['wb', '五笔', 'wubi.ywvim'],
"             \['py', '拼音', 'pinyin.ywvim'],
"             \]
" let g:ywvim_py = { 'helpim':'wb', 'gb':0 }
" let g:ywvim_zhpunc = 1
" let g:ywvim_listmax = 5
" let g:ywvim_esc_autoff = 1
" let g:ywvim_autoinput = 0
" let g:ywvim_circlecandidates = 1
" let g:ywvim_helpim_on = 0
" let g:ywvim_matchexact = 0
" let g:ywvim_gb = 1
" let g:ywvim_preconv = 'g2b'
" let g:ywvim_conv = ''
" let g:ywvim_lockb = 0
" let g:ywvim_theme = 'dark'
" " }}}

" Citation configuration {{{
" To use bibtex database
" let g:citation_vim_mode = "bibtex"
" let g:citation_vim_bibtex_file = "c:/Users/jiaho/OneDrive\ -\ PersonalOnedrive/3-Literatures/BibTex.bib"
" " To use Zotero database.
" let g:citation_vim_mode='zotero'
" let g:citation_vim_zotero_folder='c:/Users/jiaho/Dropbox\ (Personal)/3-Literatures/Zotero'
" let g:citation_vim_cache_path='"c:/Program\ Files/Vim/Citation"'
" Set citation suffix and prefix. Pandoc markdown style is the default.
" let g:citation_vim_outer_prefix="["
" let g:citation_vim_inner_prefix="@"
" let g:citation_vim_suffix="]"
" " Set a unite leader:
" nmap <leader>u [unite]
" nnoremap [unite] <nop>
" " To insert a citation:
" nnoremap <silent>[unite]c :<C-u>Unite -buffer-name=citation -start-insert -default-action=append citation/title<cr>
" " To immediately open a file or url from a citation under the cursor:
" nnoremap <silent>[unite]co :<C-u>Unite -input=<C-R><C-W> -default-action=start -force-immediately citation/file<cr>
" }}}

" Nvim-R插件设置 {{{
autocmd FileType r setlocal formatoptions+=tm
autocmd FileType r setlocal foldmethod=marker
let Rout_more_colors = 1
let R_latexcmd = '-xelatex -bibtex-cond -synctex=1 -interaction=nonstopmode'
let R_openpdf = 1
let R_openhtml = 1
let R_show_args = 1
let r_syntax_folding = 0
let rmd_syn_hl_chuck = 1
let R_save_win_pos = 1
let R_arrange_windows = 1
let R_objbr_opendf = 1
let R_objbr_openlist = 0
let R_nvimpager = "horizontal"
let R_assign = 2
let R_source_args = "print.eval = TRUE, encoding = 'UTF-8'"
" Auto quit R console when closing gVim.
autocmd VimLeave * if exists("g:SendCmdToR") && string(g:SendCmdToR) != "function('SendCmdToR_fake')" | call RQuit("nosave") | endif
" let R_cmd = "
" }}}

" Vim-airline {{{
set laststatus=2   " Always show the statusline
set t_Co=256       " Explicitly tell Vim that the terminal supports 256 colors
let g:airline#extensions#tabline#enabled = 1
set guifont=DroidSansMonoForPowerline\ NF:h12
set guifontwide=SimHei:h14
let g:airline_powerline_fonts = 1
let g:Powerline_symbols="fancy"
" }}}

" " syntastic: 语法检查 {{{
" nmap <Leader>ck :SyntasticCheck<CR>
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_check_on_open = 1
" let g:syntastic_error_symbol='>>'
" let g:syntastic_warning_symbol='>'
" let g:syntastic_check_on_wq=0
" let g:syntastic_enable_highlighting=1
" let g:syntastic_always_populate_loc_list = 0
" let g:syntastic_auto_loc_list = 0
" " }}}

" ultisnips {{{
let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsEditSplit="vertical"
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsListSnippets="<s-tab>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<c-j>"
let g:UltiSnipsSnippetDirectories=['~/vim/ultisnips', 'UltiSnips']
let g:UltiSnipsSnippetDir=['UltiSnips']
" }}}

" " FastFold设置 {{{
" let g:tex_fold_enabled=1
" let g:vimsyn_folding='af'
" let g:xml_syntax_folding = 1
" let g:php_folding = 1
" let g:perl_fold = 1
" let g:fastfold_savehook = 0
" nmap <F8> <Plug>(FastFoldUpdate)
" " }}}

" VimTex设置 {{{
let g:tex_indent_items = 0
let g:tex_indent_brace = 1
let g:tex_flavor = "latex"
let g:vimtex_quickfix_open_on_warning = 1
let g:vimtex_enabled = 1
let g:vimtex_complete_enabled = 1
let g:vimtex_complete_close_braces = 0
let g:vimtex_view_general_viewer = 'SumatraPDF'
let g:vimtex_view_general_options= '-reuse-instance -forward-search @tex @line @pdf'
let g:vimtex_view_general_options_latexmk = '-reuse-instance'
let g:vimtex_latexmk_options = '-xelatex -bibtex-cond -synctex=1 -f -interaction=nonstopmode'
let g:vimtex_fold_enabled = 1
let g:vimtex_text_obj_enabled = 1
let g:vimtex_indent_enabled = 1
let g:vimtex_motion_matchparen=0
"let g:vimtex_view_general_viewer = 'SumatraPDF -reuse-instance '
"                \ . '-inverse-search "gvim --servername ' . v:servername
"                \ . ' --remote-send \"^<C-\^>^<C-n^>'
"                \ . ':drop \%f^<CR^>:\%l^<CR^>:normal\! zzzv^<CR^>'
"                \ . ':execute ''drop '' . fnameescape(''\%f'')^<CR^>'
"                \ . ':\%l^<CR^>:normal\! zzzv^<CR^>'
"                \ . ':call remote_foreground('''.v:servername.''')^<CR^>\""'
" }}}

" Commentary设置 {{{
autocmd FileType tex setlocal commentstring=%\ %s
autocmd FileType idf setlocal commentstring=![HJ]\ %s
" }}}

" Neocomplete设置 {{{
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 2
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }
" Define keyword.
 if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
 endif
 let g:neocomplete#keyword_patterns['default'] = '\h\w*'

 if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
  endif
  let g:neocomplete#sources#omni#input_patterns.tex =
        \ '\v\\%('
        \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
        \ . '|hyperref\s*\[[^]]*'
        \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|%(include%(only)?|input)\s*\{[^}]*'
        \ . '|\a*(gls|Gls|GLS)(pl)?\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
        \ . '|includepdf%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . '|includestandalone%(\s*\[[^]]*\])?\s*\{[^}]*'
        \ . ')'
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()
" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
"  "For no inserting <CR> key.
"  return pumvisible() ? "\<C-y>" : "\<CR>"
  endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" }}}

" Vim-Easy-Align 设置 {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" }}}

" Rainbow设置 {{{
" 开启配对括号
runtime macros/matchit.vim
let g:rainbow_active=1
" }}}

" Vim-Pandoc设置 {{{
let g:pandoc#formatting#mode = 'h'
" let g:pandoc#formatting#smart_autoformat_on_cursormoved = 1
let g:pandoc#formatting#textwidth = 80
let g:pandoc#commandlatex_engine = "xelatex"
let g:pandoc#modules#disabled = ["spell", "formatting"]
let g:pandoc#toc#position = "left"
let g:pandoc#spell#default_langs = ["en_us"]
let g:pandoc#after#modules#enabled = ["ultisnips", "tablemode"]
let g:pandoc#folding#mode = "syntax"
let g:pandoc#folding#fold_yaml = 1
let b:pandoc_biblio_bibs = ["c:/Users/jiaho/OneDrive\ -\ PersonalOnedrive/3-Literatures/BibTex.bib"]
let g:pandoc#completion#bib#mode = 'citeproc'
let g:pandoc#biblio#use_bibtool = 1
let g:pandoc#syntax#conceal#use = 1
let g:pandoc#toc#close_after_navigating = 0
" }}}

" AsyncRun设置{{{
" nnoremap <silent> <Leader>ppd :AsyncRun
"             \ pandoc --standalone --smart
"             \ --filter pandoc-crossref
"             \ --filter pandoc-citeproc
"             \ --from markdown+east_asian_line_breaks
"             \ --latex-engine=xelatex -N
"             \ --template=c:\Program\ Files\Vim\template\default.latex
"             \ --output $(VIM_FILENOEXT).pdf %:p <CR>

nnoremap <silent> <Leader>ppd :AsyncRun
            \ pandoc --standalone --smart
            \ --filter pandoc-crossref
            \ --natbib
            \ --from markdown+east_asian_line_breaks
            \ --latex-engine=xelatex -N
            \ --template=c:\Program\ Files\Vim\template\default.latex
            \ --output $(VIM_FILENOEXT).pdf %:p <CR>

nnoremap <silent> <Leader>pbe :AsyncRun
            \ pandoc --standalone --smart
            \ --filter pandoc-crossref
            \ --filter pandoc-citeproc
            \ --from markdown+east_asian_line_breaks
            \ --latex-engine=xelatex -N
            \ --template=c:\Program\ Files\Vim\template\default.beamer
            \ --output $(VIM_FILENOEXT).pdf %:p <CR>

nnoremap <silent> <Leader>pht :AsyncRun
            \ pandoc --standalone --smart
            \ --filter pandoc-crossref
            \ --filter pandoc-citeproc
            \ --from markdown+east_asian_line_breaks -s -S -N --toc
            \ --css=c:\Program\ Files\Vim\pandoc_css\pandoc.css
            \ --output $(VIM_FILENOEXT).html %:p <CR>

nnoremap <silent> <Leader>pdc :AsyncRun
            \ pandoc --standalone --smart
            \ --filter pandoc-crossref
            \ --filter pandoc-citeproc
            \ --from markdown+east_asian_line_breaks
            \ --reference-docx=c:\Program\ Files\Vim\template\default.docx
            \ --output $(VIM_FILENOEXT).docx %:p <CR>

nnoremap <silent> <Leader>pte :AsyncRun
            \ pandoc --standalone --smart
            \ --filter pandoc-crossref
            \ --natbib
            \ --from markdown+east_asian_line_breaks
            \ --latex-engine=xelatex -N
            \ --template=c:\Program\ Files\Vim\template\default.latex
            \ --output $(VIM_FILENOEXT).tex %:p <CR>

" nnoremap <silent> <Leader>pte :AsyncRun
"             \ pandoc --standalone --smart
"             \ --filter pandoc-crossref
"             \ --filter pandoc-citeproc
"             \ --from markdown+east_asian_line_breaks
"             \ --latex-engine=xelatex -N
"             \ --template=c:\Program\ Files\Vim\template\default.latex
"             \ --output $(VIM_FILENOEXT).tex %:p <CR>

nnoremap <silent> <Leader>ptp :AsyncRun
            \ latexmk -xelatex -bibtex-cond -f -interaction=nonstopmode
            \ $(VIM_FILENOEXT).tex <CR>

" nnoremap <silent> <Leader>ptp :AsyncRun
"             \ pandoc --standalone --smart
"             \ pandoc -s -S
"             \ --filter pandoc-crossref
"             \ --filter pandoc-citeproc
"             \ --from markdown+east_asian_line_breaks
"             \ --latex-engine=xelatex -N
"             \ --template=c:\Program\ Files\Vim\template\default.latex
"             \ --output $(VIM_FILENOEXT).pdf $(VIM_FILENOEXT).tex <CR>
" "}}}

" Vim-Table-Mode设置 {{{
let g:table_mode_corner_corner = "|"
let g:table_mode_header_fillchar = "-"
let g:table_mode_motion_up_map = ',tk'
let g:table_mode_motion_down_map = ',tj'
let g:table_mode_motion_left_map = ',th'
let g:table_mode_motion_right_map = ',tl'
" }}}

" Vim-Shell设置 {{{
let g:shell_fullscreen_items = 'mT'
let g:shell_fullscreen_always_on_top = 0
" }}}

" VimShell设置 {{{
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_prompt = "> "
let g:vimshel_secondary_prompt = ">> "
" }}}

" Vim-Session设置 {{{
" 阻止Vim-Session自动询问是否要自动保存和载入
let g:session_autoload = 0
let g:session_autosave = 0
" }}}

" Unite 设置 {{{
" https://www.reddit.com/r/vim/comments/26470p/how_are_you_using_unitevim/
function! UniteWrapper(action, arguments)
  return ":\<C-u>silent! Unite -no-split -toggle " . a:action . " " . a:arguments . "\<CR>"
endfunction

" <Space>f - show files in the current working directory and project tree
nnoremap <space>f :UniteWithBufferDir file file_rec/async:! file_mru directory_mru file/new -no-split -toggle -buffer-name=unite-files -start-insert<cr>

" <Space>r - show recent files and directories
nnoremap <space>r :Unite file_mru directory_mru -start-insert<cr>

" <Space>b - show open buffers
    nnoremap <expr> <space>b UniteWrapper('buffer', '-buffer-name=unite-buffers -start-insert')

" <Space>s - show session buffers
    nnoremap <expr> <space>s UniteWrapper('session', '-buffer-name=unite-session -start-insert')

" 使用<Leader>sn来建立新的session
    nnoremap <expr><Leader>sn UniteWrapper('session/new', '-buffer-name=unite-session -start-insert')

" <Space>y - show yank history
    nnoremap <expr> <space>y UniteWrapper('history/yank', '-buffer-name=unite-yank-history')

" <Space>o - show file outline
    nnoremap <expr> <space>o UniteWrapper('outline', '-start-insert')

" <Space>l - show line source
    nnoremap <expr> <space>l UniteWrapper('line', '-start-insert')

"<Space>e to launch everything in Uite.
    nnoremap <expr> <space>e UniteWrapper('everything', '-buffer-name=unite-everything -start-insert')
" Unite Everything search
" A number of output from everything
let g:unite_source_everything_limit = 100
" Setting 1 makes everything do a full path search.
let g:unite_source_everything_full_path_search = 0
" Setting 1 makes everything search with basic POSIX regular expression.
let g:unite_source_everything_posix_regexp_search = 0
" Setting 1 makes everything sort result by full path.
let g:unite_source_everything_sort_by_full_path = 0
" Setting 1 makes everything do case sensitive search.
let g:unite_source_everything_case_sensitive_search = 0
" Minimum characters to start search in :Unite everything/async.
let g:unite_source_everything_async_minimum_length = 2
" }}}

" VimFiler configuration {{{
" Set VimFiler as default explorer
let g:vimfiler_as_default_explorer = 1
" Let VimFiler do not ignore dot files
let g:vimfiler_ignore_pattern = []
" Enable insert mode in VimFiler
autocmd FileType vimfiler nnoremap <buffer>i :VimFilerPrompt<CR>
" Disable VimFiler safe mode
nnoremap <space>v :VimFilerBufferDir -force-hide -horizontal -toggle -status<CR>
call vimfiler#custom#profile('default', 'context', {
          \ 'safe' : 0,
          \ })
" }}}

" }}}

" EnergyPlus IDF 相关 {{{
" 设置idf折叠方式 {{{2
augroup ft_idf
    au!
au BufRead,BufNewFile *.idf set filetype=idf
au BufRead,BufNewFile *.imf set filetype=idf
augroup END
"}}}
"}}}

" 使用主题 {{{2
set background=dark
" let g:solarized_bold = 0
" colorscheme solarized
let g:molokai_original = 1
set t_Co=256
colorscheme monokai
" }}}
