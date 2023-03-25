""" PLUGINS

" call plug#begin() 
" 	Plug 'preservim/nerdtree'
" call plug#end()

" Nerdtree settings
" Ignore .swp files 
" let NERDTreeIgnore = ['\.swp$','\.class$','\.o$','\.pyc$']
" 
" " Start NERDTree and put the cursor back in the other window.
" autocmd VimEnter * NERDTree | wincmd p
" 
" " Exit Vim if NERDTree is the only window remaining in the only tab.
" autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" 
" " If another buffer tries to replace NERDTree, put it in the other window, and
" bring back NERDTree.
" autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~'NERD_tree_\d\+' && winnr('$') > 1 | \ let buf=bufnr() | buffer# | execute 'normal! \<C-W>w' | execute 'buffer'.buf | endif

" Pathogen
execute pathogen#infect()

" Syntastic settings
" set statusline+=%#warningmsg#%{SyntasticStatuslineFlag()}%* enable if vimairline is disabled
" let g:syntastic_check_on_open=1 	" check for errors when opening file
" let g:syntastic_cursor_column=0 	" idk, apparently better performance
" let g:syntastic_enable_signs=0 		" remove >> from next to line count
" let g:syntastic_loc_list_height=5 	" height (in #lines) of :Errors window

" vim-airline settings
let g:airline_theme="minimalist"
let airline#extensions#syntastic#stl_format_err = '%E{[%fe(#%e)]}'
let airline#extensions#syntastic#stl_format_warn = '%W{[%fw(#%w)]}'
let g:airline_section_z = airline#section#create(['%3p%% %l:%3v'])



""" SETTINGS

" Set language
set langmenu=en_US.UTF-8
let $LANG="en_US"
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Line numbers
set number
set relativenumber

" Status bar
set laststatus=2

" Theme
syntax on
colorscheme codedark
set t_Co=256

" Make backspace work
set backspace=indent,eol,start

" Make indentation work
" Indent with \t
" set softtabstop=0 noexpandtab

" Indent with 4x<space>
set expandtab
set softtabstop=4

" Global indent settings
set autoindent
set smartindent
set shiftwidth=4
set tabstop=4

" Make search work
set hlsearch
set ignorecase
set smartcase
set nocompatible



""" MAPPINGS

" inoremap { {}<Esc>i
" inoremap ( ()<Esc>i
" inoremap [ []<Esc>i

map <F1> :term<enter><C-w>L
map <F12> :vsplit $MYVIMRC<CR><C-w>L

nnoremap <S-h> 5h
nnoremap <S-j> 5j
nnoremap <S-k> 5k
nnoremap <S-l> 5l
nnoremap <silent> <C-l> :nohlsearch<CR>
nnoremap % :%s/
nnoremap E dd

vnoremap <C-c> "*y
vnoremap <C-v> "*p

" Misc
set mouse-=a
