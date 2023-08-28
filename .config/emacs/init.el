(setq package-user-dir "~/.config/emacs/packages"
      use-package-always-ensure t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Install packages
;; Show hints for keybinds
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :init
  (setq which-key-side-window-location 'bottom
 	which-key-sort-order #'which-key-key-order-alpha
 	which-key-sort-uppercase-first nil
 	which-key-add-column-padding 1
 	which-key-max-display-columns nil
 	which-key-min-display-lines 6
 	which-key-side-window-slot -10
 	which-key-side-window-max-height 0.25
 	which-key-idle-delay 0.8
 	which-key-max-description-length 25
 	which-key-allow-imprecise-window-fit t
 	which-key-separator " → " ))

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
 	evil-want-C-u-scroll t
 	evil-vsplit-window-below t
 	evil-want-fine-undo 'fine
 	evil-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package general
  :ensure t
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
    "<backspace>" 'dired-up-directory))

;; Terminal
(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :init
  (eshell-syntax-highlighting-global-mode))

(use-package vterm
  :ensure t
  :init
  (setq shell-file-name "/bin/zsh"
	vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :init
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project))

;; Completion
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
	      ("C-<backspace>" . (lambda (arg)
				   (interactive "p")
				   (if minibuffer-completing-file-name
				       (if (string-match-p "/." (minibuffer-contents))
					   (zap-up-to-char (- arg) ?/)
					 (delete-minibuffer-contents))
				     (backward-kill-word arg)))))
  :init
  (vertico-mode))

(use-package consult
  :ensure t)

(use-package marginalia
  :ensure t
  :init
  (setq marginalia-align 'right)
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless flex)))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  (global-corfu-mode))

;; Mode-Line
(use-package mood-line
  :ensure t
  :init
  (mood-line-mode))

;; See lines as indentation guides
(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 100)
  :hook (prog-mode . highlight-indent-guides-mode))

;; Preview colors while editing
(use-package rainbow-mode
  :ensure t
  :hook org-mode prog-mode help-mode)

;; Git
(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c gn" . 'git-gutter:next-hunk)
	 ("C-c gp" . 'git-gutter:previous-hunk))
  :init
  (setq git-gutter:update-interval 0.50))

;; Language support
(use-package eros
  :ensure t
  :init
  (eros-mode))

(use-package lua-mode
  :ensure t)

(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(racket)))

(use-package geiser-racket
  :ensure t
  :after geiser)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-l"))

(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

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
   '(which-key vterm-toggle vertico rainbow-mode orderless mood-line marginalia magit lua-mode lsp-mode highlight-indent-guides git-gutter general geiser-racket evil-collection eshell-syntax-highlighting eros corfu consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
