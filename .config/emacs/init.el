;; Setup for use-package
(require 'package)
(require 'use-package-ensure)

(setq package-user-dir "~/.config/emacs/packages"
      use-package-always-ensure t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install packages
;; Show hints for keybinds
(use-package which-key
  :init
  (which-key-mode))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-keybinding nil
 	evil-want-C-u-scroll t
 	evil-vsplit-window-below t
 	evil-want-fine-undo 'fine
 	evil-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

;; Keybinds
(use-package general
  :init
  (general-evil-setup)

  (general-def '(normal visual emacs)
    ;; Buffer commands
    "C-c C-i" 'ibuffer
    "C-c C-b" 'consult-buffer
    "C-c C-n" 'next-buffer
    "C-c C-p" 'previous-buffer
    "C-c C-k" 'kill-buffer-and-window
    "C-c C-e" 'eval-buffer

    ;; Misc
    "C-c C-t" 'vterm-toggle
    "C-h F" 'describe-face
    "M-r" 'shell-command
    "C-s" 'consult-line)

  ;; Keybinds for specific vim modes
  (general-def 'normal
    "/" 'comment-line)
  
  (general-def 'visual
    "/" 'comment-or-uncomment-region
    "<tab>" 'indent-region)

  (general-def 'insert
    ;; Navigate text in insert mode
    "C-h" 'backward-char
    "C-j" 'next-line
    "C-k" 'previous-line
    "C-l" 'forward-char

    ;; Force completion
    "C-n" 'completion-at-point)

  ;; Dired keybinds
  (general-def ('normal 'insert 'emacs) dired-mode-map
    "a" 'dired-create-empty-file
    "A" 'dired-create-directory
    "r" 'dired-do-rename
    "d" 'dired-do-delete
    "<RET>" 'dired-single-buffer
    "<backspace>" 'dired-single-up-directory))

;; Dired
(use-package dired-single)

;; Terminal
(use-package eshell-syntax-highlighting
  :after esh-mode
  :init
  (eshell-syntax-highlighting-global-mode))

(use-package vterm
  :init
  (setq shell-file-name "/bin/zsh"
	vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :init
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project))

;; Completion
(use-package vertico
  :bind (:map minibuffer-local-map
	      ("C-<backspace>" . (lambda (arg)
				   (interactive "p")
				   (if minibuffer-completing-file-name
				       (if (string-match-p "/." (minibuffer-contents))
					   (zap-up-to-char (- arg) ?/)
					 (delete-minibuffer-contents))
				     (backward-kill-word arg)))))
  :init
  (vertico-mode)
  (vertico-mouse-mode))

(use-package consult)

(use-package marginalia
  :init
  (setq marginalia-align 'right)
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex)))

(use-package corfu
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (setq corfu-auto t
	corfu-auto-delay 0.05
	corfu-quit-no-match 'separator)
  (global-corfu-mode))

;; Mode-Line
(use-package mood-line
  :init
  (mood-line-mode))

;; See lines as indentation guides
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 100)
  :hook (prog-mode . highlight-indent-guides-mode))

;; Preview colors while editing
(use-package rainbow-mode
  :hook org-mode prog-mode help-mode)

;; Git
(use-package magit)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c gn" . 'git-gutter:next-hunk)
	 ("C-c gp" . 'git-gutter:previous-hunk))
  :init
  (setq git-gutter:update-interval 0.50))

;; Language support
(use-package eros
  :init
  (eros-mode))

(use-package lua-mode)

(use-package geiser
  :init
  (setq geiser-active-implementations '(racket)))

(use-package geiser-racket
  :after geiser)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :init
  (defun corfu-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (setq lsp-keymap-prefix "C-l")
  :hook
  (lsp-completion-mode . corfu-setup-completion))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp))))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; Set theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'yellowbeans t)

;; Set fonts
(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 140
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height 140
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Cantarell"
  :height 110
  :weight 'medium)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-14"))

;; Set some sensible settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(setq display-line-numbers-type 'relative

      scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position 't

      inhibit-startup-message t
      initial-scratch-message nil

      dired-listing-switches "-AhgGoF --group-directories-first --color=auto"

      backup-directory-alist '((".*" . "~/.cache/emacs")))

;; Autogenerated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-java which-key vterm-toggle vertico rainbow-mode orderless mood-line marginalia magit lua-mode lsp-mode highlight-indent-guides git-gutter general geiser-racket evil-collection eshell-syntax-highlighting eros corfu consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
