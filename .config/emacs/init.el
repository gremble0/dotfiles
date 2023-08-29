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
  :config
  (which-key-mode))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-keybinding nil
 	evil-want-C-u-scroll t
 	evil-vsplit-window-below t
 	evil-want-fine-undo 'fine
 	evil-undo-system 'undo-redo)
  :config
  ;; Insert mode keybinds
  (evil-define-key 'insert 'global
    (kbd "C-h") 'evil-backward-char
    (kbd "C-h") 'evil-backward-char
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line
    (kbd "C-l") 'evil-forward-char
    (kbd "C-n") 'completion-at-point)

  ;; Motion state keybinds (visual, normal, emacs)
  (evil-define-key 'motion 'global
    (kbd "C-c C-i") 'ibuffer
    (kbd "C-c C-b") 'consult-buffer
    (kbd "C-c C-n") 'next-buffer
    (kbd "C-c C-p") 'previous-buffer
    (kbd "C-c C-k") 'kill-buffer-and-window
    (kbd "C-c C-e") 'eval-buffer
    (kbd "C-c C-r") 'eval-region

    (kbd "C-c C-t") 'vterm-toggle
    (kbd "C-h F") 'describe-face
    (kbd "M-r") 'shell-command
    (kbd "C-s") 'consult-line)

  ;; Visual state keybinds
  (evil-define-key 'visual 'global
    (kbd "/") 'comment-or-uncomment-region
    (kbd "<tab>") 'indent-region)
    
  ;; Normal state keybinds
  (evil-define-key 'normal 'global
    (kbd "/") 'comment-line
    (kbd "<tab>") 'evil-indent-line)

  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Dired
(use-package dired-single)

;; Terminal
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package vterm
  :init
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :init
  (setq vterm-toggle-fullscreen-p nil
	vterm-toggle-scope 'project))

;; Completion
(use-package vertico
  :bind
  (:map minibuffer-local-map
	("C-<backspace>" . (lambda (arg)
			     (interactive "p")
			     (if minibuffer-completing-file-name
				 (if (string-match-p "/." (minibuffer-contents))
				     (zap-up-to-char (- arg) ?/)
				   (delete-minibuffer-contents))
			       (backward-kill-word arg)))))
  :config
  (vertico-mode)
  (vertico-mouse-mode))

(use-package consult)

(use-package marginalia
  :init
  (setq marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex)))

(use-package corfu
  :init
  (setq corfu-auto t
	corfu-auto-delay 0.05
	corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode))

;; Mode-Line
(use-package mood-line
  :config
  (mood-line-mode))

;; See lines as indentation guides
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 100)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; Preview colors while editing
(use-package rainbow-mode
  :hook
  org-mode prog-mode help-mode)

;; Git
(use-package magit)

(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :bind
  (("C-c gn" . 'git-gutter:next-hunk)
   ("C-c gp" . 'git-gutter:previous-hunk))
  :init
  (setq git-gutter:update-interval 0.50))

;; Language support
(use-package eros
  :config
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

;; TODO: Make this prettier and more easily extensible - dolist maybe?
(use-package lsp-pyright
  :hook
  (python-mode . lsp))

(use-package lsp-java
  :hook
  (java-mode . lsp))
  
(use-package tree-sitter
  :config
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

      shell-file-name "/bin/zsh"

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
