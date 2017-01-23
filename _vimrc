scriptencoding utf-8

" INITIALIZATION ==========================================================={{{1
" Identify platform {{{2
let g:MAC = has('macunix')
let g:LINUX = has('unix') && !has('macunix') && !has('win32unix')
let g:WINDOWS = has('win32') || has('win64')
" }}}2

" Windows Compatible {{{2
" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if g:WINDOWS
    set runtimepath=$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim,$HOME/.vim/after
endif
" }}}2

" Chinese {{{2
let $LANG='zh_CN.UTF-8'
set langmenu=zh_CN
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,cp936,gb18030,big5,euc-jp,euc-kr,latin1
set fileencoding=utf-8
set termencoding=utf-8
language messages zh_CN.utf-8
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
augroup ft_vim
    au!
au FileType vim setlocal foldmethod=marker
augroup END

" windows GUI界面乱码设置
if g:WINDOWS
  " Set extra options when running in GUI mode
  set guifont=DroidSansMonoForPowerline\ NF:h12
  set guifontwide=SimHei:h14
  set guitablabel=%M\ %t
  set linespace=2
  set noimdisable
endif
" Chinese (END) }}}2
" INITIALIZATION (END)======================================================}}}1

" PLUGINS =================================================================={{{1
" Auto installing Vim-Plug {{{2
let iCanHazDein=1
let dein_readme = expand($HOME. '/.vim/dein/repos/github.com/Shougo/dein.vim/README.md')
if !filereadable(dein_readme)
    echo "Installing Dein..."
    echo ""
    silent !git clone https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim c:/Users/hongy/vimfiles/autoloa/plug.vim
    let iCanHazDein=0
endif
" }}}2

" Auto install missing plugins on startup {{{2
" https://github.com/junegunn/vim-plug/wiki/extra#automatically-install-missing-plugins-on-startup
autocmd VimEnter *
            \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
            \|   echom '[space-vim]Some layers need to install the missing plugins first!'
            \|   PlugInstall --sync | q
            \| endif
call plug#begin(expand($HOME.'/vimfiles/plugged'))
" }}}2

" Airline {{{2
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" }}}2

" Dark themes {{{2
Plug 'tomasr/molokai'
" Dark themes (END) }}}2

" Light themes {{{2
Plug 'joedicastro/vim-github256'
" Light themes (END) }}}2

" Better Defaults {{{2
Plug 'tpope/vim-rsi'
Plug 'mhinz/vim-startify'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'terryma/vim-multiple-cursors'
Plug 'easymotion/vim-easymotion',           { 'on': [
    \   '<Plug>(easymotion-prefix)',
    \   '<Plug>(easymotion-bd-f)',
    \   '<Plug>(easymotion-overwin-f)',
    \   '<Plug>(easymotion-overwin-f2)',
    \   '<Plug>(easymotion-bd-jk)',
    \   '<Plug>(easymotion-overwin-line)',
    \   '<Plug>(easymotion-bd-w)',
    \   '<Plug>(easymotion-overwin-w)',
    \   ] }
Plug 'danro/rename.vim',               { 'on' : 'Rename' }
Plug 'ntpeters/vim-better-whitespace', { 'on': 'StripWhitespace' }
" Better Defaults (END) }}}2

" Text-Align {{{2
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align',  { 'on': [ '<Plug>(EasyAlign)', 'EasyAlign' ] }
" Text-Align (END) }}}2

" Programming {{{2
Plug 'luochen1990/rainbow'
Plug 'editorconfig/editorconfig-vim', { 'on': 'EditorConfigReload' }
Plug 'matze/vim-move', { 'on': [ 
    \   '<Plug>MoveBlockDown',
    \   '<Plug>MoveBlockUp',
    \   '<Plug>MoveLineDown',
    \   '<Plug>MoveLineUp',
    \   ]}
Plug 'junegunn/rainbow_parentheses.vim', { 'for': ['R', 'lisp', 'clojure', 'scheme'] }
Plug 'Raimondi/delimitMate', { 'on': [] }
" Refer to https://github.com/junegunn/vim-plug/wiki/faq
" Load on nothing
Plug 'SirVer/ultisnips', { 'on': [] } | Plug 'honza/vim-snippets', { 'on': [] }
Plug 'majutsushi/tagbar'
" Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plug 'Chiel92/vim-autoformat',          { 'on': 'Autoformat'}
Plug 'nathanaelkane/vim-indent-guides', { 'on': 'IndentGuidesToggle'}
Plug 'skywind3000/asyncrun.vim',        { 'on': ['AsyncRun!', 'AsyncRun'] }
Plug 'tpope/vim-commentary'
" Programming (END) }}}2

" Git {{{2
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'gregsexton/gitv'
" Git (END) }}}2

" Unite {{{2
Plug 'Shougo/unite.vim'
Plug 'Shougo/unite-outline'
Plug 'tsukkee/unite-help'
Plug 'ujihisa/unite-colorscheme'
Plug 'ujihisa/unite-locate'
Plug 'thinca/vim-unite-history'
Plug 'osyo-manga/unite-filetype'
Plug 'osyo-manga/unite-quickfix'
Plug 'osyo-manga/unite-fold'
Plug 'tacroe/unite-mark'
Plug 'Shougo/vimfiler'
Plug 'Shougo/junkfile.vim'
Plug 'joedicastro/unite-cmdmatch'
Plug 'kopischke/unite-spell-suggest'
Plug 'Shougo/neomru.vim'
" Unite (END) }}}2

" Auto-completion {{{2
Plug 'Shougo/neocomplete.vim'
" Auto-completion (END) }}}2

" Easy Text Manipulation {{{2
" Plug 'Shougo/neomru.vim'
Plug 'vim-scripts/utl.vim'
Plug 'tpope/vim-speeddating'
Plug 'sjl/gundo.vim'
Plug 'salsifis/vim-transpose'
Plug 'kshenoy/vim-signature'
" Plug 'kana/vim-textobj-entire'
" Plug 'kana/vim-textobj-indent'
" Plug 'kana/vim-textobj-lastpat'
" Plug 'kana/vim-textobj-line'
" Plug 'kana/vim-textobj-underscore'
" Plug 'kana/vim-textobj-function'
" Plug 'kana/vim-textobj-user'
Plug 'kshenoy/vim-signature'
" Easy Text Manipulation (END) }}}2

" R Markdown {{{2
Plug 'jalvesaq/Nvim-R'
Plug 'rafaqz/citation.vim'
" R Markdown (END) }}}2

" Initialize plugin system
call plug#end()
" END PLUGINS ==============================================================}}}1

" GUI ======================================================================{{{1
" Color Scheme {{{2
set t_Co=256       " Use 256 colors
colorscheme molokai
highlight ColorColumn guibg=SlateGray
" Color Scheme (END) }}}2

" Gui Options {{{2
" Max GVim When Starting
au GUIEnter * simalt ~x
set guioptions-=m
set guioptions-=r        " Hide the right scrollbar
set guioptions-=L        " Hide the left scrollbar
set guioptions-=T
set guioptions-=e
set shortmess+=c
" No annoying sound on errors
set noerrorbells
set novisualbell
set visualbell t_vb=
" set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<
set list
" IME Settings
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
" SpaceEmacs-like Statusline
silent! set showtabline=1
" silent! set tabline=%!MyTabLine()
" set statusline=%!MyStatusLine()
syntax on                      " Syntax highlighting
highlight clear SignColumn  " SignColumn should match background
" highlight clear LineNr      " Current line number row will have same background color in relative mode
" }}}2

" Vim-Better-Whitespace {{{2
highlight ExtraWhitespace ctermbg=197
" Vim-Better-Whitespace (END) }}}2

" Vim-Startify {{{2
let g:startify_custom_header = [
        \'                                             _',
        \'         ___ _ __   __ _  ___ ___     __   _(_)_ __ ___',
        \'        / __| -_ \ / _- |/ __/ _ \____\ \ / / | -_ - _ \',
        \'        \__ \ |_) | (_| | (_|  __/_____\ V /| | | | | | |',
        \'        |___/ .__/ \__._|\___\___|      \_/ |_|_| |_| |_|',
        \'            |_|',
        \]
augroup SPACEVIM_START
  autocmd!
  autocmd VimEnter *
            \   if !argc()
            \|      Startify
            \|  endif
augroup END
let g:startify_list_order = [
        \ ['   Recent Files:'],
        \ 'files',
        \ ['   Project:'],
        \ 'dir',
        \ ['   Sessions:'],
        \ 'sessions',
        \ ['   Bookmarks:'],
        \ 'bookmarks',
        \ ['   Commands:'],
        \ 'commands',
        \ ]
" Vim-Startify (END) }}}2
" GUI (END) ================================================================}}}1

" BETTER DEFAULTS =========================================================={{{1
filetype plugin indent on      " Automatically detect file types
set fileformats=unix,dos,mac        " Use Unix as the standard file type
set clipboard+=unnamed
set cursorline              " Highlight current line
set number                  " Line numbers on
set nocompatible
set winaltkeys=no
set foldenable
set foldlevel=0
set foldlevelstart=99
set wrap
set nobackup
set noswapfile
set nowritebackup
" set nowrap         " Do not wrap long lines
set textwidth=80
set formatoptions+=mM
set colorcolumn=81
set autoindent                 " Indent at the same level of the previous line
set autoread                   " Automatically read a file changed outside of vim
set backspace=indent,eol,start " Backspace for dummies
"set complete-=i                " Exclude files completion
set display=lastline           " Show as much as possible of the last line
set history=10000              " Maximum history record
set hlsearch                   " Highlight search terms
set incsearch                  " Find as you type search
set laststatus=2               " Always show status line
set mouse=a                    " Automatically enable mouse usage
set smarttab                   " Smart tab
set ttyfast                    " Faster redrawing
set viminfo+=!                 " Viminfo include !
set wildmenu                   " Show list instead of just completing
set shortmess=atOI " No help Uganda information, and overwrite read messages to avoid PRESS ENTER prompts
set ignorecase     " Case sensitive search
set smartcase      " Case sensitive when uc present
set scrolljump=5   " Line to scroll when cursor leaves screen
set scrolloff=3    " Minumum lines to keep above and below cursor
set shiftwidth=4   " Use indents of 4 spaces
set tabstop=4      " An indentation every four columns
set softtabstop=4  " Let backspace delete indent
set splitright     " Puts new vsplit windows to the right of the current
set splitbelow     " Puts new split windows to the bottom of the current
set autowrite      " Automatically write a file when leaving a modified buffer
set mousehide      " Hide the mouse cursor while typing
set hidden         " Allow buffer switching without saving
set ruler          " Show the ruler
set showcmd        " Show partial commands in status line and Selected characters/lines in visual mode
set showmode       " Show current mode in command-line
set showmatch      " Show matching brackets/parentthesis
set matchtime=5    " Show matching time
set report=0       " Always report changed lines
set linespace=0    " No extra spaces between rows
set expandtab      " Tabs are spaces, not tabs
set winminheight=0
set wildmode=list:longest,full
set wildignore+=*swp,*.class,*.pyc,*.png,*.jpg,*.gif,*.zip
set wildignore+=*/tmp/*,*.o,*.obj,*.so     " Unix
set wildignore+=*\\tmp\\*,*.exe            " Windows

set ttymouse=xterm2
" Resize the divisions if the Vim window size changes
au VimResized * exe "normal! \<c-w>="
set whichwrap+=<,>,h,l  " Allow backspace and cursor keys to cross line boundaries
" BETTER DEFAULTS (END) =================================================== }}}1

" FILETYPE AU =============================================================={{{1
" Restore cursor position when opening file {{{2
augroup SPACEVIM_BASIC
    autocmd BufReadPost *
                \ if line("'\"") > 1 && line("'\"") <= line("$") |
                \   execute "normal! g`\"" |
                \ endif

    autocmd BufReadPre *
                \   if getfsize(expand("%")) > 10000000 |
                \   syntax off |
                \   endif

    autocmd BufEnter * call MyLastWindow()
augroup END
" }}}2

" Commentary strings for different file types {{{2
augroup plugin_commentary
    au!
    au FileType python setlocal commentstring=#%s
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType puppet setlocal commentstring=#\ %s
    au FileType xquery setlocal commentstring=(:\ %s\ :)
    au FileType idf setlocal commentstring=!##\ %s
augroup END
" }}}2

" RainbowParentheses only enable for R {{{2
augroup rainbow_r
    autocmd!
    autocmd FileType r RainbowParentheses
augroup END
" }}}2

" EnergyPlus file type {{{2
augroup ft_idf
    au!
    au BufRead,BufNewFile *.idf set filetype=idf
    au BufRead,BufNewFile *.imf set filetype=idf
augroup END
" }}}2

" Call compile function when Asyncrun starts {{{2
augroup SPACEVIM_ASYNCRUN
    autocmd!
    autocmd User AsyncRunStart call asyncrun#quickfix_toggle(15, 1)
augroup END
" }}}2

" Enable omni completion {{{2
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
" }}}2
" FILETYPE AU ==============================================================}}}1

" PLUGIN SETUP ============================================================={{{1
" Airline {{{2
let g:airline_theme='powerlineish'
let g:airline_powerline_fonts=1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline#extensions#tabline#enabled = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#buffer_min_count = 1
" }}}2
" NeoComplete {{{2
" Note: This option must be set in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
            \ 'default' : '',
            \ 'vimshell' : expand($HOME.'/vimfiles/tmp/neocomplete/.vimshell_hist'),
            \ 'scheme' : expand($HOME.'/vimfiles/tmp/neocomplete/.gosh_completions')
            \ }
" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"
" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1
" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplete#enable_auto_select = 1
"let g:neocomplete#disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"
" Enable heavy omni completion.
" if !exists('g:neocomplete#sources#omni#input_patterns')
"     let g:neocomplete#sources#omni#input_patterns = {}
" endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
" }}}2

" Vim-Multiple-Cursors {{{2
let g:multi_cursor_next_key='<C-j>'
let g:multi_cursor_prev_key='<C-k>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'
" Vim-Multiple-Cursors (END) }}}2

" UltiSnips {{{2
if has_key(g:plugs, 'ultisnips')
    " UltiSnips will be loaded only when tab is first pressed in insert mode
    if !exists(':UltiSnipsEdit')
        inoremap <silent> <Plug>(tab) <c-r>=plug#load('ultisnips')?UltiSnips#ExpandSnippet():''<cr>
        imap <tab> <Plug>(tab)
    endif
endif

" Set ultisnips triggers
let g:UltiSnipsSnippetDirectories=['UltiSnips']
let g:UltiSnipsSnippetsDir = [$HOME . 'vimfiles/plugged/plugged/vim-snippets/UltiSnips/']
let g:UltiSnipsListSnippets = '<C-Tab>'
let g:UltiSnipsJumpForwardTrigger = '<Tab>'
let g:UltiSnipsJumpBackwardTrigger = '<S-Tab>'
" Fix tab conflict with YCM
let g:UltiSnipsExpandTrigger = '<nop>'
let g:ulti_expand_or_jump_res = 0
function! ExpandSnippetOrCarriageReturn()
    let l:snippet = UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res > 0
        return l:snippet
    else
        return "\<CR>"
    endif
endfunction
" UltiSnips (END) }}}2

" Gitv {{{2
let g:Gitv_OpenHorizontal = 'auto'
let g:Gitv_WipeAllOnClose = 1
let g:Gitv_DoNotMapCtrlKey = 1
" Gitv (END) }}}2

" Vim-GitGutter {{{2
let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_modified = '~'
let g:gitgutter_sign_removed = '-'
let g:gitgutter_sign_removed_first_line = '^^'
let g:gitgutter_sign_modified_removed = 'ww'
let g:gitgutter_override_sign_column_highlight = 0
" Vim-GitGutter (END) }}}2

" Gundo {{{2
let g:gundo_preview_bottom = 1
" Gundo (END) }}}2

" IndentLine {{{2
let g:indentLine_enabled = 0
let g:indentLine_enabled=1
let g:indentLine_char = '┊'
let g:indentLine_color_term = 239
let g:indentLine_concealcursor='vc'      " default 'inc'
let g:indentLine_fileTypeExclude = ['help', 'startify', 'vimfiler']
" IndentLine (END) }}}2

" Vim-Indent-Guides {{{2
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
" }}}2

" DelimitMate {{{2
    let g:delimitMate_expand_cr=1
" }}}}2

" NeoMRU {{{
" let g:neomru#file_mru_path = expand($HOME.'vimfiles/tmp/neomru/file')
" let g:neomru#directory_mru_path = expand($HOME.'vimfiles/tmp/neomru/directory')
" NeoMRU (END) }}}2

" Unite {{{2
" Unite Basics {{{3
let g:unite_data_directory = expand($HOME.'/vimfiles/tmp/unite')
let g:unite_source_history_yank_enable = 1
let g:unite_enable_start_insert = 0
let g:unite_enable_short_source_mes = 0
let g:unite_force_overwrite_statusline = 0
let g:unite_ignorecase = 1
let g:unite_smartcase = 1
let g:unite_prompt = '>>> '
let g:unite_marked_icon = '✓'
let g:unite_candidate_icon = '∘'
let g:unite_winheight = 15
let g:unite_update_time = 200
let g:unite_split_rule = 'botright'
let g:unite_source_buffer_time_format = '(%d-%m-%Y %H:%M:%S) '
let g:unite_source_file_mru_time_format = '(%d-%m-%Y %H:%M:%S) '
let g:unite_source_directory_mru_time_format = '(%d-%m-%Y %H:%M:%S) '
" Unite Basics }}}3

" Unite Menu {{{3
" Unite Menu Misc {{{4
let g:unite_source_menu_menus = {}
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file_mru,file_rec,file_rec/async,grep,locate',
            \ 'ignore_pattern', join(['\.git/', 'tmp/', 'bundle/'], '\|'))
" pt
if executable('pt')
    let g:unite_source_grep_command='pt'
    let g:unite_source_grep_default_opts='--nocolor --nogroup --smart-case'
    let g:unite_source_grep_recursive_opt=''
    let g:unite_source_grep_search_word_highlight = 1
endif
" Junk
let g:junkfile#directory=expand($HOME."/vimfiles/tmp/junk")
" VimFiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_tree_leaf_icon = '├'
let g:vimfiler_tree_opened_icon = '┐'
let g:vimfiler_tree_closed_icon = '─'
let g:vimfiler_file_icon = '┄'
let g:vimfiler_marked_file_icon = '✓'
let g:vimfiler_readonly_file_icon = '✗'
let g:vimfiler_force_overwrite_statusline = 0
let g:vimfiler_time_format = '%d-%m-%Y %H:%M:%S'
let g:vimfiler_data_directory = expand($HOME.'/vimfiles/tmp/vimfiler')
" }}}4

" [menu]x : menu.edition {{{4
let g:unite_source_menu_menus.x = {
            \ 'description' : ' text            ⌘   [menu]x',
            \}
let g:unite_source_menu_menus.x.command_candidates = [
            \['▷   show-hidden-chars',
            \'set list!'],
            \['▷   x d ➞  delete-trailing-whitespaces                            SPC x d',
            \'StripWhitespace'],
            \['▷   a | ➞  align-repeat-bar                                       SPC a |',
            \'Tabularize /|'],
            \['▷   a = ➞  align-repeat-equal                                     SPC a =',
            \'Tabularize /^[^=]*\zs='],
            \['▷   s c ➞  cancel-highlight-of-searched-result                    SPC s c',
            \'nohl'],
            \['▷   t p ➞  toggle-paste-mode                                      SPC t p',
            \'setlocal paste!'],
            \]
" }}}4

" [menu]f : menu.files&dirs {{{4
let g:unite_source_menu_menus.f = {
            \ 'description' : ' files & dirs     ⌘  [menu]f',
            \}
let g:unite_source_menu_menus.f.command_candidates = [
            \['▷ open file                                                  ⌘    SPC f f',
            \'Unite -start-insert file'],
            \['▷ open more recently used files                              ⌘    SPC f r',
            \'Unite file_mru'],
            \['▷ open file with recursive search                            ⌘    SPC f F',
            \'Unite -start-insert file_rec/async'],
            \['▷ edit new file                                                   SPC f n',
            \'Unite -start-insert file/new'],
            \['▷ search directory                                                SPC f d',
            \'Unite -start-insert directory'],
            \['▷ search recently used directories                                SPC f R',
            \'Unite -start-insert directory_mru'],
            \['▷ search directory with recursive search                          SPC f D',
            \'Unite -start-insert directory_rec/async'],
            \['▷ make new directory                                              SPC f N',
            \'Unite -start-insert directory/new'],
            \['▷ change working directory',
            \'Unite -default-action=lcd directory'],
            \['▷ know current working directory',
            \'Unite output:pwd'],
            \['▷ junk files',
            \'Unite junkfile/new junkfile'],
            \['▷ save as root                                               ⌘    :w!!',
            \'exe "write !sudo tee % >/dev/null"'],
            \['▷ quick save                                                 ⌘    SPC f s',
            \'update'],
            \['▷ open vimfiler                                              ⌘    SPC f V',
            \'VimFiler'],
            \]
" }}}4

" [menu]s : menu.search {{{4
let g:unite_source_menu_menus.s = {
            \ 'description' : ' search        ⌘     [menu]s',
            \}
let g:unite_source_menu_menus.s.command_candidates = [
            \['▷ grep (pt → grep)                                     ⌘          SPC s p',
            \'Unite -no-quit grep'],
            \['▷ Everything                                           ⌘          SPC s e',
            \'Unite -start-insert everything'],
            \['▷ locate                                                          SPC s L',
            \'Unite -start-insert locate'],
            \['▷ vimgrep (very slow)',
            \'Unite vimgrep'],
            \['▷ search line                                                ⌘    SPC s l',
            \'Unite -auto-preview -start-insert line'],
            \['▷ search word under the cursor                               ⌘    SPC s w',
            \'UniteWithCursorWord -no-split -auto-preview line'],
            \['▷ search outlines & tags (ctags)                             ⌘    SPC s o',
            \'Unite -vertical -winwidth=40 -direction=topleft -toggle outline'],
            \['▷ search marks                                                    SPC s m',
            \'Unite -auto-preview mark'],
            \['▷ search folds                                                    SPC s f',
            \'Unite -vertical -winwidth=30 -auto-highlight fold'],
            \['▷ search changes                                                  SPC s c',
            \'Unite change'],
            \['▷ search jumps                                                    SPC s j',
            \'Unite jump'],
            \['▷ search undos                                                    SPC s u',
            \'Unite undo'],
            \['▷ search yanks                                                    SPC s y',
            \'Unite history/yank'],
            \['▷ search commands (history)                                   ⌘   SPC s C',
            \'Unite history/command'],
            \['▷ searches history                                     ⌘          SPC s s',
            \'Unite history/search'],
            \['▷ search registers                                                SPC s r',
            \'Unite register'],
            \['▷ search messages                                                 SPC s M',
            \'Unite output:messages'],
            \['▷ search undo tree (gundo)                                     ⌘  SPC s U',
            \'GundoToggle'],
            \]
" }}}4

" [menu]n : menu.buffers {{{4
let g:unite_source_menu_menus.b = {
            \ 'description' : ' buffers     ⌘       [menu]b',
            \}
let g:unite_source_menu_menus.b.command_candidates = [
            \['▷ buffers                                                    ⌘    SPC b b',
            \'Unite buffer'],
            \['▷ buffer previous                                                 SPC b p',
            \'bprevious'],
            \['▷ buffer nest                                                     SPC b n',
            \'bnext'],
            \['▷ buffer first                                                    SPC b f',
            \'bfirst'],
            \['▷ buffer last                                                     SPC b l',
            \'blast'],
            \['▷ buffer delete                                                   SPC b d',
            \'bdelete'],
            \['▷ buffer wripe                                                    SPC b k',
            \'bwripe'],
            \['▷ tabs                                                       ⌘    SPC b t',
            \'Unite tab'],
            \['  move half page down                                             SPC d',
            \'normal <Leader>d'],
            \['  move half page up                                               SPC u',
            \'normal <Leader>u'],
            \['▷ quickfix                                                        SPC b q',
            \'Unite quickfix'],
            \]
" }}}4

" [menu]w : menu.windows {{{4
let g:unite_source_menu_menus.w = {
            \ 'description' : ' windows     ⌘       [menu]w',
            \}
let g:unite_source_menu_menus.w.command_candidates = [
            \['  window delete                                                   SPC w d',
            \'close'],
            \['▷ toggle quickfix window                                     ⌘    SPC w q',
            \'normal ,q'],
            \]
" }}}4

" [menu]S : menu.spellchecking {{{4
let g:unite_source_menu_menus.S = {
            \ 'description' : ' spell checking      [menu]S',
            \}
let g:unite_source_menu_menus.S.command_candidates = [
            \['▷ spell checking in English                                  ⌘    SPC S e',
            \'setlocal spell spelllang=en'],
            \['▷ turn off spell checking                                    ⌘    SPC S p',
            \'setlocal nospell'],
            \['▷ jumps to next bad spell word and show suggestions          ⌘    SPC S n',
            \'normal ,sc'],
            \['▷ jumps to next bad spell word                               ⌘    SPC S N',
            \'normal ,sn'],
            \['▷ suggestions                                                ⌘    SPC S s',
            \'normal ,sp'],
            \['▷ add word to dictionary                                     ⌘    SPC S a',
            \'normal ,sa'],
            \]
" }}}4

" [menu]p : menu.plugins {{{4
let g:unite_source_menu_menus.p = {
            \ 'description' : ' plugins          ⌘  [menu]s',
            \}
let g:unite_source_menu_menus.p.command_candidates = [
            \['▷  install-plugin                                                 (vim-plug)',
            \'PlugInstall'],
            \['▷  clean-plugin                                                   (vim-plug)',
            \'PlugClean'],
            \['▷  update-plugin                                                  (vim-plug)',
            \'PlugUpdate'],
            \['▷  show-plugin-status                                             (vim-plug)',
            \'PlugStatus'],
            \]
" }}}4

" [menu]t : menu.toggle {{{4
let g:unite_source_menu_menus.t = {
            \ 'description' : ' toggle           ⌘  [menu]t',
            \}
let g:unite_source_menu_menus.t.command_candidates = [
            \['▷  tagbar                                                         <F6>',
            \'TagbarToggle'],
            \['▷  indent-line                                                    ',
            \'IndentGuidesToggle'],
            \['▷  syntastic                                                      ',
            \'SyntasticToggleMode'],
            \['   open cmd in current dir                                        SPC s-quote',
            \'shell'],
            \['   pastemode                                                      SPC t p',
            \':setlocal paste!'],
            \['▷ toggle search results highlight                               ⌘ ,eq',
            \'set invhlsearch'],
            \['▷ toggle line numbers                                        ⌘    ,l',
            \'call ToggleRelativeAbsoluteNumber()'],
            \['▷ toggle wrapping                                            ⌘    ,ew',
            \'call ToggleWrap()'],
            \['▷ toggle auto-completion state (manual → disabled → auto)    ⌘    ,ea',
            \'call ToggleNeoCompleteAutoSelect()'],
            \['▷ show hidden chars                                          ⌘    ,eh',
            \'set list!'],
            \['▷ toggle fold                                                ⌘    /',
            \'normal za'],
            \['▷ open all folds                                             ⌘    zR',
            \'normal zR'],
            \['▷ close all folds                                            ⌘    zM',
            \'normal zM'],
            \['▷ copy to the clipboard                                      ⌘    ,y',
            \'normal ,y'],
            \['▷ paste from the clipboard                                   ⌘    ,p',
            \'normal ,p'],
            \['▷ toggle paste mode                                          ⌘    ,P',
            \'normal ,P'],
            \['▷ remove trailing whitespaces                                ⌘    ,et',
            \'normal ,et'],
            \]
" }}}4

" [menu]g : menu.git {{{4
let g:unite_source_menu_menus.g = {
            \ 'description' : ' git commands        [menu]g',
            \}
let g:unite_source_menu_menus.g.command_candidates = [
            \['▷ git viewer             (gitv)                              ⌘    SPC g v',
            \'normal ,gv'],
            \['▷ git viewer - buffer    (gitv)                              ⌘    SPC g V',
            \'normal ,gV'],
            \['▷ git status             (fugitive)                          ⌘    SPC g s',
            \'Gstatus'],
            \['▷ git diff               (fugitive)                          ⌘    SPC g d',
            \'Gdiff'],
            \['▷ git commit             (fugitive)                          ⌘    SPC g c',
            \'Gcommit'],
            \['▷ git log                (fugitive)                          ⌘    SPC g l',
            \'exe "silent Glog | Unite -no-quit quickfix"'],
            \['▷ git log - all          (fugitive)                          ⌘    SPC g L',
            \'exe "silent Glog -all | Unite -no-quit quickfix"'],
            \['▷ git blame              (fugitive)                          ⌘    SPC g b',
            \'Gblame'],
            \['▷ git add/stage          (fugitive)                          ⌘    SPC g w',
            \'Gwrite'],
            \['▷ git checkout           (fugitive)                          ⌘    SPC g o',
            \'Gread'],
            \['▷ git rm                 (fugitive)                          ⌘    SPC g R',
            \'Gremove'],
            \['▷ git mv                 (fugitive)                          ⌘    SPC g m',
            \'exe "Gmove " input("destino: ")'],
            \['▷ git push               (fugitive, buffer output)           ⌘    SPC g p',
            \'Git! push'],
            \['▷ git pull               (fugitive, buffer output)           ⌘    SPC g P',
            \'Git! pull'],
            \['▷ git command            (fugitive, buffer output)           ⌘    SPC g i',
            \'exe "Git! " input("comando git: ")'],
            \['▷ git edit               (fugitive)                          ⌘    SPC g E',
            \'exe "command Gedit " input(":Gedit ")'],
            \['▷ git grep               (fugitive)                          ⌘    SPC g g',
            \'exe "silent Ggrep -i ".input("Pattern: ") | Unite -no-quit quickfix'],
            \['▷ git grep - messages    (fugitive)                          ⌘    SPC g g m',
            \'exe "silent Glog --grep=".input("Pattern: ")." | Unite -no-quit quickfix"'],
            \['▷ git grep - text        (fugitive)                          ⌘    SPC g g t',
            \'exe "silent Glog -S".input("Pattern: ")." | Unite -no-quit quickfix"'],
            \['▷ git init                                                   ⌘    SPC g n',
            \'Unite output:echo\ system("git\ init")'],
            \['▷ git cd                 (fugitive)',
            \'Gcd'],
            \['▷ git lcd                (fugitive)',
            \'Glcd'],
            \['▷ git browse             (fugitive)                          ⌘    SPC g B',
            \'Gbrowse'],
            \]
" }}}4

" [menu]v : menu.vim {{{4
let g:unite_source_menu_menus.v = {
            \ 'description' : ' vim              ⌘  [menu]v',
            \}
let g:unite_source_menu_menus.v.command_candidates = [
            \['▷  .vimrc                                                         [menu] v v',
            \'e ~/_vimrc'],
            \['▷  .spacevim                                                      [menu] v s',
            \'e ~/.spacevim'],
            \['▷  vim-help                                                       [menu] v h',
            \'Unite help -start-insert'],
            \['▷  vim-commands                                                   [menu] v c',
            \'Unite command -start-insert'],
            \['▷  vim-mappings                                                   [menu] v m',
            \'Unite mapping -start-insert'],
            \['▷  vim-functions                                                  [menu] v f',
            \'Unite function -start-insert'],
            \['▷  vim-runtimepath                                                [menu] v r',
            \'Unite runtimepath -start-insert '],
            \['▷  vim-command-output                                             [menu] v o',
            \'Unite output'],
            \]
" }}}4
" }}}3
" Unite (END) }}}2

" Tagbar {{{2
let g:tagbar_autofocus = 1
let g:tagbar_sort = 0
let g:tagbar_ctags_bin = "c:/ctags/ctags.exe"
" }}}2

" Citation {{{3
let g:citation_vim_mode="zotero"
let g:citation_vim_zotero_path="c:/Dropbox/3-Literatures/Zotero/"
let g:citation_vim_cache_path='~/.cache/citation'
" }}}3
" PLUGIN SETUP (END) =======================================================}}}1

" KEY BINDINGS ============================================================={{{1
" [a-z] or Special {{{2
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
" Quit visual mode
vnoremap v <Esc>
" Move to the start of line
nnoremap H ^
vnoremap H ^
" Move to the end of line
nnoremap L $
vnoremap L $
" Redo
nnoremap U <C-r>
" Yank to the end of line
nnoremap Y y$
" Quick command mode
nnoremap ; :
nnoremap : ;
" Toggle folding: \
nnoremap \ za
vnoremap \ za
" Visual shifting: <>
vnoremap < <gv
vnoremap > >gv
" }}}2
" <Leader> {{{2
let mapleader=" "
let maplocalleader = ","
" }}}2
" <Leader><Leader> {{{2
map <Leader><Leader> <Plug>(easymotion-prefix)
" }}}2
" <Leader>a {{{2
" Tabular {{{3
nmap <Leader>a& :Tabularize /&<CR>
vmap <Leader>a& :Tabularize /&<CR>
nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
nmap <Leader>a=> :Tabularize /=><CR>
vmap <Leader>a=> :Tabularize /=><CR>
nmap <Leader>a: :Tabularize /:<CR>
vmap <Leader>a: :Tabularize /:<CR>
nmap <Leader>a:: :Tabularize /:\zs<CR>
vmap <Leader>a:: :Tabularize /:\zs<CR>
nmap <Leader>a, :Tabularize /,<CR>
vmap <Leader>a, :Tabularize /,<CR>
nmap <Leader>a,, :Tabularize /,\zs<CR>
vmap <Leader>a,, :Tabularize /,\zs<CR>
nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
nmap <Leader>a! :Tabularize /!-<CR>
vmap <Leader>a! :Tabularize /!-><CR>
" }}}3
" }}}2
" <Leader>b {{{2
nnoremap <silent><Leader>bh :Startify<CR>
nnoremap <silent><Leader>bb :Unite -silent -start-insert buffers<CR>
nnoremap <silent><Leader>bp :bprevious<CR>
nnoremap <silent><Leader>bn :bnext<CR>
nnoremap <silent><Leader>bf :bfirst<CR>
nnoremap <silent><Leader>bl :blast<CR>
nnoremap <silent><Leader>bd :bdelete<CR>
nnoremap <silent><Leader>bw :bwripe<CR>
nnoremap <silent><Leader>bt :Unite tab<CR>
nnoremap <silent><Leader>bq :Unite quickfix<CR>
" <Leader>b[1-9] move to buffer [1-9] {{{3
for s:i in range(1, 9)
    execute 'nnoremap <Leader>b' . s:i . ' :b' . s:i . '<CR>'
endfor
" }}}3
" }}}2
" <Leader>c {{{2

" nmap <Leader>c <Plug>CommentaryLine
" xmap <Leader>c <Plug>Commentary
" }}}2
" <Leader>d {{{2
" for the diffmode
noremap <silent> <Leader>du :diffupdate<CR>
noremap <silent> <Leader>dq :Gdiffoff<CR>
" }}}2
" <Leader>f {{{2
nnoremap <silent><Leader>ff :UniteWithBufferDir -silent -start-insert file file_mru<CR>
nnoremap <silent><Leader>fF :UniteWithBufferDir -silent -start-insert file_rec/async<CR>
nnoremap <silent><Leader>fn :UniteWithBufferDir -silent -start-insert file/new<CR>
nnoremap <silent><Leader>fd :UniteWithBufferDir -silent -start-insert directory directory_mru<CR>
nnoremap <silent><Leader>fD :UniteWithBufferDir -silent -start-insert directory_rec/async<CR>
nnoremap <silent><Leader>fV :VimFiler<CR>
nnoremap <silent><Leader>fv :e $MYVIMRC<CR>
nnoremap <Leader>fR :source $MYVIMRC<CR>
nnoremap <Leader>fs :update<CR>
" Fold 
nnoremap <Leader>f0 :set foldlevel=0<CR>
nnoremap <Leader>f1 :set foldlevel=1<CR>
nnoremap <Leader>f2 :set foldlevel=2<CR>
nnoremap <Leader>f3 :set foldlevel=3<CR>
nnoremap <Leader>f4 :set foldlevel=4<CR>
nnoremap <Leader>f5 :set foldlevel=5<CR>
nnoremap <Leader>f6 :set foldlevel=6<CR>
nnoremap <Leader>f7 :set foldlevel=7<CR>
nnoremap <Leader>f8 :set foldlevel=8<CR>
nnoremap <Leader>f9 :set foldlevel=9<CR>
" }}}2
" <Leader>g {{{2
nnoremap <silent> <Leader>gn :Unite output:echo\ system("git\ init")<CR>
nnoremap <silent> <Leader>gs :Gstatus<CR>
nnoremap <silent> <Leader>gd :Gdiff<CR>
nnoremap <silent> <Leader>gc :Gcommit<CR>
nnoremap <silent> <Leader>gb :Gblame<CR>
nnoremap <silent> <Leader>gl :Glog<CR>
nnoremap <silent> <Leader>gp :Git! push<CR>
nnoremap <silent> <Leader>gP :Git! pull<CR>
nnoremap <silent> <Leader>gr :Gread<CR>
nnoremap <silent> <Leader>gw :Gwrite<CR>
nnoremap <silent> <Leader>ge :Gedit<CR>
nnoremap <silent> <Leader>gR :Gremove<CR>
nnoremap <silent> <Leader>gm :Gmove<Space>
nnoremap <silent> <Leader>gB :Gbrowse<CR>
nnoremap <silent> <Leader>gi :Git!<Space>
nnoremap <silent> <Leader>gE :Gedit<Space>
" Mnemonic _i_nteractive
nnoremap <silent> <Leader>gi :Git add -p %<CR>
nnoremap <silent> <Leader>gg :SignifyToggle<CR>
nnoremap <silent> <Leader>gl :exe "silent Glog <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <silent> <Leader>gL :exe "silent Glog -- <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <silent> <Leader>gt :!tig<CR>:redraw!<CR>
nnoremap <silent> <Leader>gS :exe "silent !shipit"<CR>:redraw!<CR>
nnoremap <silent> <Leader>gg :exe 'silent Ggrep -i '.input("Pattern: ")<Bar>Unite
            \ quickfix -no-quit<CR>
nnoremap <silent> <Leader>ggm :exe 'silent Glog --grep='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>
nnoremap <silent> <Leader>ggt :exe 'silent Glog -S='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>
nnoremap <silent> <Leader>ggc :silent! Ggrep -i<Space>
nnoremap <silent> <leader>gv :Gitv --all<CR>
nnoremap <silent> <leader>gV :Gitv! --all<CR>
vnoremap <silent> <leader>gV :Gitv! --all<CR>
" }}}2
" <Leader>m {{{2
" nnoremap [menu] <Nop>
" nmap <Leader>m [menu]
" [menu]m : toggle {{{3
nnoremap <silent><Leader>m :Unite -silent -winheight=40 menu<CR>
" }}}3
" [menu]b : menu.buffers {{{3
nnoremap <silent><Leader>mb :Unite -silent menu:b<CR>
" }}}3
" [menu]w : menu.windows {{{3
nnoremap <silent><Leader>mw :Unite -silent menu:w<CR>
" }}}3
" [menu]S : menu.spellchecking {{{3
nnoremap <silent><Leader>mS :Unite -silent menu:S<CR>
" }}}3
" [menu]p : menu.plugins {{{3
nnoremap <silent><Leader>mp :Unite -silent menu:p<CR>
" }}}3
" [menu]t : menu.toggle {{{3
nnoremap <silent><Leader>mt :Unite -silent menu:t<CR>
" }}}3
" [menu]g : menu.git {{{3
nnoremap <silent><Leader>mg :Unite -silent -winheight=29 -start-insert menu:g<CR>
" }}}3
" [menu]v : menu.vim {{{3
nnoremap <silent><Leader>mv :Unite -silent menu:v<CR>
" }}}3
" [menu]x : menu.edition {{{3
nnoremap <silent><Leader>mx :Unite -silent -winheight=20 menu:x<CR>
" }}}3
" [menu]f : menu.files&dirs {{{3
nnoremap <silent><Leader>mf :Unite -silent -winheight=20 menu:f<CR>
" }}}3
" [menu]s : menu.search {{{3
nnoremap <silent><Leader>ms :Unite -silent menu:s<CR>
" }}}3
" }}}2
" <Leader>n {{{2
nnoremap n nzz
nnoremap N Nzz
" }}}2
" <Leader>j {{{2
" Move to {char}
map  <Leader>jj <Plug>(easymotion-bd-f)
nmap <Leader>jj <Plug>(easymotion-overwin-f)
" Move to {char}{char}
nmap <Leader>jJ <Plug>(easymotion-overwin-f2)
" Jump to line
map <Leader>jl <Plug>(easymotion-bd-jk)
nmap <Leader>jl <Plug>(easymotion-overwin-line)
" Jump to word
map  <Leader>jw <Plug>(easymotion-bd-w)
nmap <Leader>jw <Plug>(easymotion-overwin-w)
" }}}2
" <Leader>p {{{2
nnoremap <Leader>p "*p
" toggle paste mode
nnoremap <Leader>P :set invpaste<CR>
" }}}2
" <Leader>q {{{2
nnoremap <Leader>q  :q<CR>
nnoremap <Leader>Q  :qa<CR>
" }}}2
" <Leader>r {{{2
nnoremap <Leader>rl :call NumberToggle()<cr>
" }}}2
" <Leader>s {{{2
" Search result highlight countermand
nnoremap <Leader>sc :nohlsearch<CR>
nnoremap <silent><Leader>sp :Unite -no-quit grep<CR>
nnoremap <silent><Leader>sl :Unite -silent -start-insert line<CR>
nnoremap <silent><Leader>sL :Unite -silent -start-insert locate<CR>
nnoremap <silent><Leader>sw :Unite -silent -auto-preview -start-insert line<CR>
nnoremap <silent><Leader>so :Unite -vertical -winwidth=40 -direction=topleft -toggle outline<CR>
nnoremap <silent><Leader>sb :Unite -auto-preview mark<CR>
nnoremap <silent><Leader>sb :Unite -vertical -winwidth=30 -auto-highlight fold<CR>
nnoremap <silent><Leader>sj :Unite jump<CR>
nnoremap <silent><Leader>su :Unite undo<CR>
" nnoremap <silent><Leader>st :Unite -toggle grep:%::FIXME|TODO|NOTE|XXX|COMBAK|@todo<CR>
nnoremap <silent><Leader>sy :Unite history/yank<CR>
nnoremap <silent><Leader>sC :Unite history/command<CR>
nnoremap <silent><Leader>ss :Unite history/search<CR>
nnoremap <silent><Leader>sr :Unite register<CR>
nnoremap <silent><Leader>sM :Unite output:messages<CR>
" turn on the spell checking and set the English language
nnoremap <Leader>Se :setlocal spell spelllang=en<CR>
" turn off the spell checking
nnoremap <Leader>So :setlocal nospell <CR>
" jump to the next bad spell word
nnoremap <Leader>Sn ]s
" suggest words
" nnnoremap <Leader>sp z=
nnoremap <Leader>Sp :Unite spell_suggest<CR>
" jump to the next bad spell word and suggests a correct one
" nnnoremap <Leader>sc ]sz=
nnoremap <Leader>Sc ]s :Unite spell_suggest<CR>
" add word to the dictionary
nnoremap <Leader>Sa zg
" }}}2
" <Leader>t {{{2
" Vim-Indent-Guides
nnoremap <Leader>ti :IndentGuidesToggle<CR>
nnoremap <Leader>tt :TagbarToggle<CR>
nnoremap <Leader>tq :ToggleQuickfix<CR>
nnoremap <Leader>tg :GitGutterToggle<CR>
nnoremap <Leader>tL :IndentLinesToggle<CR>
" Toggle pastemode
nnoremap <Leader>tp :setlocal paste!<CR>
" }}}2
" <Leader>u {{{2
nnoremap <Leader>u :GundoToggle<CR>
" }}}2
" <Leader>w {{{2
nnoremap <Leader>ww <C-W>w
nnoremap <Leader>wr <C-W>r
nnoremap <Leader>wd <C-W>c
nnoremap <Leader>wq <C-W>q
nnoremap <Leader>wj <C-W>j
nnoremap <Leader>wk <C-W>k
nnoremap <Leader>wh <C-W>h
nnoremap <Leader>wl <C-W>l
nnoremap <Leader>wH <C-W>5<
nnoremap <Leader>wL <C-W>5>
nnoremap <Leader>wJ :resize +5<CR>
nnoremap <Leader>wK :resize -5<CR>
nnoremap <Leader>w= <C-W>=
nnoremap <Leader>ws <C-W>s
nnoremap <Leader>w- <C-W>s
nnoremap <Leader>wv <C-W>v
nnoremap <Leader>w\| <C-W>v
nnoremap <Leader>w2 <C-W>v
nnoremap <Leader>w/ <C-W>v
" }}}2
" <Leader>x {{{2
nnoremap <Leader>xd :StripWhitespace<CR>
" Start interactive EasyAlign in visual mode (e.g. vipxa)
xmap <Leader>xa <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. xaip)
nmap <Leader>xa <Plug>(EasyAlign)
" }}}2
" <Leader>y {{{2
nnoremap <Leader>y "*y
" }}}2
" <Leader>[1-9]{{{2
for s:i in range(1, 9)
    execute 'nnoremap <Leader>' . s:i . ' :' . s:i . 'wincmd w<CR>'
endfor
" }}}2
" <Leader>Special {{{2
" Quit Vim without saving
nnoremap <Leader>`` :qa!<CR>
" Open shell in vim
map <Leader>' :shell<CR>
" }}}2
" F[n] Key {{{2
" Asyncrun
nnoremap <F5> :call <SID>compile_and_run()<CR>
nnoremap <F6> :TagbarToggle<CR>
inoremap <F6> <ESC>:TagbarToggle<CR>
" }}}2
" Special Key {{{2
" Use Ctrl-Tab to switch tab {{{3
nnoremap    <C-Tab>  :tabnext<CR>
nnoremap    <S-Tab>  :tabprev<CR>
" }}}3
" Vim-Move: <Alt> {{{3
" vim-move config
" for terms that send Alt as Escape sequence
" see http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" for why the <F20> hack. Keeps Esc from waiting for other keys to exit visual
set <F20>=j
set <F21>=k
vmap <F20> <Plug>MoveBlockDown
vmap <F21> <Plug>MoveBlockUp
nmap <F20> <Plug>MoveLineDown
nmap <F21> <Plug>MoveLineUp
" }}}3
" UltiSnips: <CR> {{{3
inoremap <expr> <CR> pumvisible() ? "<C-R>=ExpandSnippetOrCarriageReturn()<CR>" : "\<CR>"
" }}}3
" NeoComplete: <Ctrl>[glrh], <Tab>, <BackSpace> {{{3
" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()
" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" }}}3
" Easy move using <Ctrl>[aedhjkl] {{{3
" Insert mode shortcut
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>
" Bash like
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-d> <Delete>
" Command mode shortcut
cnoremap <C-h> <left>
cnoremap <C-j> <Down>
cnoremap <C-k> <Up>
cnoremap <C-l> <Right>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-d> <Delete>
        " }}}3
" }}}2
" KEY BINDINGS =============================================================}}}1

" FUNCTIONS ================================================================{{{1
" 'Source' *.vim Files {{{2
silent function! Source(file)
    if filereadable(expand(a:file))
        execute 'source ' . fnameescape(a:file)
    else
        echom '[space-vim] ' . a:file . ' does not exist, which may cause unexpected errors.'
    endif
endfunction

let g:spacevim_dir = $HOME.'/.space-vim'
let g:spacevim_core_dir = '/core'
let g:spacevim_version = '0.5.0'
" call Source(g:spacevim_dir . g:spacevim_core_dir . '/core_config.vim')
" }}}2

" NumberToggle {{{2
function! NumberToggle()
    if(&relativenumber == 1)
        set norelativenumber number
    else
        set relativenumber
    endif
endfunc
" }}}2

" QuickfixToggle {{{2
function! s:QuickfixToggle()
    for i in range(1, winnr('$'))
        let bnum = winbufnr(i)
        if getbufvar(bnum, '&buftype') == 'quickfix'
            cclose
            lclose
            return
        endif
    endfor
    copen
endfunction
command! ToggleQuickfix call <SID>QuickfixToggle()
" }}}2

" Fugitive: Gdiffoff Refined {{{2
if !exists(":Gdiffoff")
    command Gdiffoff diffoff | q | Gedit
endif
" }}}2

" NeoComplete: my_cr_function {{{2
function! s:my_cr_function()
    return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
    " For no inserting <CR> key.
    "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" }}}2

" " SpaceEmacs-like Statusline {{{2

" " MyTabLine {{{3
" function! MyTabLine()
"     let l:s = ''
"     let l:t = tabpagenr()
"     let l:i = 1
"     while l:i <= tabpagenr('$')
"         let l:buflist = tabpagebuflist(l:i)
"         let l:winnr = tabpagewinnr(l:i)
"         let l:s .= '%' . l:i . 'T'
"         let l:s .= (l:i == l:t ? '%1*' : '%2*')
"         let l:s .= ' '
"         let l:s .= 'T-' . l:i . ':'
"         let l:s .= l:winnr . '/' . tabpagewinnr(l:i,'$') .'W'
"         let l:s .= ' %*'
"         let l:s .= (l:i == l:t ? '%#TabLineSel#' : '%#TabLine#')
"         let l:bufnr = l:buflist[l:winnr - 1]
"         let l:file = bufname(l:bufnr)
"         let l:buftype = getbufvar(l:bufnr, 'buftype')
"         if l:buftype ==# 'nofile'
"             if l:file =~# '\/.'
"                 let l:file = substitute(l:file, '.*\/\ze.', '', '')
"             endif
"         else
"             let l:file = fnamemodify(l:file, ':p:t')
"         endif
"         if l:file ==# ''
"             let l:file = '[No Name]'
"         endif
"         let l:s .= l:file
"         let l:i = l:i + 1
"     endwhile
"     let l:s .= '%T%#TabLineFill#%='
"     let l:s .= (tabpagenr('$') > 1 ? '%999XX' : 'X')
"     return l:s
" endfunction
" " }}}3

" " S_buf_num {{{3
" " The decoration of statusline was stealed from
" " https://github.com/junegunn/dotfiles/blob/master/vimrc.
" " %< Where to truncate
" " %n buffer number
" " %F Full path
" " %m Modified flag: [+], [-]
" " %r Readonly flag: [RO]
" " %y Type:          [vim]
" " fugitive#statusline()
" " %= Separator
" " %-14.(...)
" " %l Line
" " %c Column
" " %V Virtual column
" " %P Percentage
" " %#HighlightGroup#
" function! S_buf_num()
"     let l:circled_num_list = ['① ', '② ', '③ ', '④ ', '⑤ ', '⑥ ', '⑦ ', '⑧ ', '⑨ ', '⑩ ',
"                 \             '⑪ ', '⑫ ', '⑬ ', '⑭ ', '⑮ ', '⑯ ', '⑰ ', '⑱ ', '⑲ ', '⑳ ']
"     if bufnr('%') > 20
"         return bufnr('%')
"     endif
"     return l:circled_num_list[bufnr('%')-1]
" endfunction
" " }}}3

" " S_buf_total_num {{{3
" function! S_buf_total_num()
"     return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
" endfunction

" function! S_file_size(f)
"     let l:size = getfsize(expand(a:f))
"     if l:size == 0 || l:size == -1 || l:size == -2
"         return ''
"     endif
"     if l:size < 1024
"         return l:size.' bytes'
"     elseif l:size < 1024*1024
"         return printf('%.1f', l:size/1024.0).'k'
"     elseif l:size < 1024*1024*1024
"         return printf('%.1f', l:size/1024.0/1024.0) . 'm'
"     else
"         return printf('%.1f', l:size/1024.0/1024.0/1024.0) . 'g'
"     endif
" endfunction
" " }}}3

" " S_full_path {{{3
" function! S_full_path()
"     if &filetype ==# 'startify'
"         return ''
"     else
"         return expand('%:p:t')
"     endif
" endfunction
" " }}}3

" " S_ale_error {{{3
" function! S_ale_error()
"     if !exists('g:loaded_ale')
"         return ''
"     endif
"     return !empty(ALEGetError())?ALEGetError():''
" endfunction
" " }}}3

" " S_ale_warning {{{3
" function! S_ale_warning()
"     if !exists('g:loaded_ale')
"         return ''
"     endif
"     return !empty(ALEGetWarning())?ALEGetWarning():''
" endfunction
" " }}}3

" " S_fugitive {{{3
" function! S_fugitive()
"     if !exists('g:loaded_fugitive')
"         return ''
"     endif
"     let l:head = fugitive#head()
"     return empty(l:head) ? '' : ' ⎇ '.l:head . ' '
" endfunction
" " }}}3

" " S_git_status {{{3
" let s:job_status = {}
" function! S_git_status()
"     if !exists('g:loaded_fugitive')
"         return ''
"     endif
"     let l:roots = values(s:job_status)
"     let l:root = fugitive#head()
"     if index(roots, root) >= 0
"         return ''
"     endif
"     let job_id = jobstart(['git-status'], {
"                 \ 'cwd': root,
"                 \ 'on_stdout': function('s:JobHandler', [root]),
"                 \ 'on_stderr': function('s:JobHandler', [root]),
"                 \ 'on_exit': function('s:JobHandler', [root])
"                 \})
"     if job_id == 0 || job_id == -1 | return '' | endif
"     let s:job_status[job_id] = root
"     return ''
" endfunction
" " }}}3

" " MyStatusLine {{{3
" function! MyStatusLine()
"     " if g:spacevim_gui_running
"     let l:buf_num = '%1* [B-%n] [W-%{winnr()}] %*'
"     " else
"     "     let l:buf_num = '%1* %{S_buf_num()} ❖ %{winnr()} %*'
"     " endif
"     let l:tot = '%2*[TOT:%{S_buf_total_num()}]%*'
"     let l:fs = '%3* %{S_file_size(@%)} %*'
"     let l:fp = '%4* %{S_full_path()} %*'
"     let l:ale_e = '%#ale_error#%{S_ale_error()}%*'
"     let l:ale_w = '%#ale_warning#%{S_ale_warning()}%*'
"     let l:git = '%6*%{S_fugitive()}%*'
"     let l:m_r_f = '%7* %m%r%y %*'
"     let l:ff = '%8* %{&ff} |'
"     let l:enc = " %{''.(&fenc!=''?&fenc:&enc).''} | %{(&bomb?\",BOM\":\"\")}"
"     let l:pos = '%l:%c%V %*'
"     let l:pct = '%9* %P %*'
"     return l:buf_num.l:tot.'%<'.l:fs.l:fp.l:git.l:ale_e.l:ale_w.
" endfunction
" " See the statusline highlightings in s:post_user_config() of core/core_config.vim

" " Note that the "%!" expression is evaluated in the context of the
" " current window and buffer, while %{} items are evaluated in the
" " context of the window that the statusline belongs to.
" " }}}3

" MyLastWindow {{{3
function! MyLastWindow()
    " if the window is quickfix/locationlist go on
    if &buftype ==# 'quickfix' || &buftype ==# 'locationlist'
        " if this window is last on screen quit without warning
        if winbufnr(2) == -1
            quit!
        endif
    endif
endfunction
" }}}3
" }}}2

" Asyncrun: comile_and_run {{{2
function! s:compile_and_run()
    exec 'w'
    if &filetype == 'c'
        exec "AsyncRun! gcc % -o %<; time ./%<"
    elseif &filetype == 'python'
        exec "AsyncRun! time python %"
    endif
endfunction
" }}}2
" FUNCTIONS (END) ==========================================================}}}1
