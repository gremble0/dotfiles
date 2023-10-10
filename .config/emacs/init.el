;;; init.el --- Main entrypoint for my personal emacs configuration

;;; Commentary:
;; This file is mainly a string of package imports with some configuration
;; done to each package + the default settings for GNU Emacs. Feel free
;; to copy anything you like

;;; Code:
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

;; Editing
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo 'fine)
  (evil-undo-system 'undo-redo)
  :config
  ;; Insert mode keybinds
  (evil-define-key 'insert 'global
    (kbd "C-h") 'evil-backward-char
    (kbd "C-h") 'evil-backward-char
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line
    (kbd "C-l") 'evil-forward-char
    (kbd "C-n") 'completion-at-point)

  ;; General keybinds mode keybinds
  (evil-define-key '(normal visual motion emacs operator replace) 'global
    (kbd "C-i") 'ibuffer
    (kbd "C-n") 'next-buffer
    (kbd "C-p") 'previous-buffer
    (kbd "C-c C-k") 'kill-buffer-and-window
    (kbd "C-c C-e") 'eval-buffer
    (kbd "C-c C-r") 'eval-region
    (kbd "C-h F") 'describe-face
    (kbd "M-r") 'shell-command)

  ;; Visual state keybinds
  (evil-define-key 'visual prog-mode-map
    (kbd "/") 'comment-or-uncomment-region
    (kbd "<tab>") 'indent-region)
    
  ;; Normal state keybinds
  (evil-define-key 'normal prog-mode-map
    (kbd "/") 'comment-line
    (kbd "<tab>") 'evil-indent-line)

  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-character-face-perc 100)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package rainbow-mode
  :hook
  org-mode prog-mode help-mode)

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-AhgGoF --group-directories-first --color=auto")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "l") 'dired-single-buffer
    (kbd "h") 'dired-single-up-directory))

(use-package dired-single)

;; Terminal
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package vterm
  :custom
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :config
  (evil-define-key '(normal visual motion emacs operator replace) 'global
    (kbd "C-t") 'vterm-toggle))

;; Completion
(use-package vertico
  :init
  ;; Mostly taken from the wiki: https://github.com/minad/vertico/wiki
  ;; Configure the default sorting function for symbols and files
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file
           (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))))

  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'dired-directory)
      file))
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
  (vertico-mouse-mode)
  (vertico-multiform-mode))

(use-package consult
  :config
  (evil-define-key '(normal visual motion emacs operator replace) 'global
    (kbd "C-b") 'consult-buffer
    (kbd "C-s") 'consult-line))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode))

;; Mode-Line
(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Git
(use-package magit)

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.50)
  :config
  (evil-define-key 'motion 'global
    (kbd "C-c n") 'git-gutter:next-hunk
    (kbd "C-c p") 'git-gutter:previous-hunk)
  (git-gutter-mode))

;; Language support
(use-package lua-mode)

(use-package geiser
  :custom
  (geiser-active-implementations '(racket))
  :config
  (evil-define-key 'motion geiser-mode-map
    (kbd "C-c C-r") 'geiser-eval-region
    (kbd "C-c C-e") 'geiser-eval-buffer))

(use-package geiser-racket
  :after geiser)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation nil)
  (gc-cons-threshold 100000000)
  (read-process-output-max 1000000)
  (lsp-keymap-prefix "C-l")
  (lsp-modeline-code-action-fallback-icon "󰌵")
  :init
  (defun corfu-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . corfu-setup-completion))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil)
  :config
  (evil-define-key 'motion lsp-mode-map
    (kbd "K") 'lsp-ui-doc-show))

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

;; Colored emoji font
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

;; Set some sensible settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(setq-default display-line-numbers-type 'relative

              scroll-conservatively 101
              scroll-margin 5
              scroll-preserve-screen-position 't

              inhibit-startup-message t
              initial-scratch-message nil

              confirm-kill-processes nil
              kill-buffer-query-functions nil

              shell-file-name "/bin/zsh"
              
              backup-directory-alist '((".*" . "~/.cache/emacs"))
              
              tab-width 4
              indent-tabs-mode nil
              indent-line-function 'insert-tab)

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
(put 'dired-find-alternate-file 'disabled nil)
