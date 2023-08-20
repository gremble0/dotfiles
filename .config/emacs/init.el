;; Bootstrap package manager
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(setq use-package-always-ensure t)

;; Block until current queue processed.
(elpaca-wait)

;; Install packages
(use-package which-key
  :init
    (which-key-mode 1)
  :config
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
  :init
  (setq evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-vsplit-window-below t
	evil-want-fine-undo 'fine
	evil-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)

  (general-def '(normal visual emacs)
    ;; Buffer commands
    "C-c C-i" 'ibuffer
    "C-c C-b" 'switch-to-buffer
    "C-c C-n" 'next-buffer
    "C-c C-p" 'previous-buffer
    "C-c C-k" 'kill-buffer-and-window
    "C-c C-e" 'eval-buffer

    ;; Help commands
    "C-h F" 'describe-face)

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

    ;; Force completion to load
    "C-n" 'completion-at-point)

  ;; Dired keybinds
  (general-def ('normal 'insert 'emacs) dired-mode-map
    "a" 'dired-create-empty-file
    "r" 'dired-do-rename
    "d" 'dired-do-delete
    "<return>" 'dired-open-file
    "<backspace>" 'dired-up-directory))

;; Terminal
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package vterm
  :config
  (setq shell-file-name "/bin/zsh"
	vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :bind (("C-c C-v" . 'vterm-toggle))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  (reusable-frames . visible)
                  (window-height . 0.4))))

;; Dired
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "nsxiv")
				("jpg" . "nsxiv")
				("png" . "nsxiv")
				("mkv" . "mpv")
				("mp4" . "mpv"))))

;; LSP integration
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

;; Completion
(defun minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :config
  (vertico-mode))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package marginalia
  :config
  (setq marginalia-align 'right)
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless flex)))

(use-package corfu
  :config
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  (global-corfu-mode))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Mode-Line
(use-package mood-line
  :config
  (mood-line-mode))

;; Preview colors while editing
(use-package rainbow-mode
  :hook org-mode prog-mode help-mode)

;; Git
(use-package magit
  :bind (("C-c gs" . 'magit-status)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c gn" . 'git-gutter:next-hunk)
	 ("C-c gp" . 'git-gutter:previous-hunk))
  :config
  (setq git-gutter:update-interval 0.50))

;; Language support
(use-package lua-mode)

;; Set theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'yellowbeans t)

;; Set fonts
(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 150
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height 150
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Cantarell"
  :height 110
  :weight 'medium)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-15"))

;; Set some sensible settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(require 'ls-lisp)
(setq display-line-numbers-type 'relative

      scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position 't

      inhibit-startup-message t
      initial-scratch-message nil

      ls-lisp-dirs-first t
      ls-lisp-use-insert-directory-program nil)

;; Defining some font faces
;; Git-gutter font faces
(defgroup git-gutter nil
  "Group for colors used by git-gutter"
  :group 'processes)

(defface git-gutter:added
  '((t :inherit default :weight bold :foreground "#60a840"))
  "Face displayed in gutter for new git hunks"
  :group 'git-gutter)

(defface git-gutter:modified
  '((t :inherit git-gutter:added :foreground "#ffa500"))
  "Face displayed in gutter for modified git hunks"
  :group 'git-gutter)

(defface git-gutter:modified
  '((t :inherit git-gutter:added :foreground "#d22b2b"))
  "Face displayed in gutter for modified git hunks"
  :group 'git-gutter)

;; Vterm font faces
(defgroup vterm nil
  "Group for colors used by vterm"
  :group 'processes)

(defface vterm-color-red
  '((t :background "#cc0000":foreground "#cc0000"))
  "Face for red colors in terminal"
  :group 'vterm)

(defface vterm-color-green
  '((t :foreground "#99ad6a"))
  "Face for green colors in terminal"
  :group 'vterm)

(defface vterm-color-yellow
  '((t :foreground "#e1b655"))
  "Face for yellow colors in terminal"
  :group 'vterm)

(defface vterm-color-blue
  '((t :foreground "#8197bf"))
  "Face for blue colors in terminal"
  :group 'vterm)

(defface vterm-color-magenta
  '((t :foreground "#75507b"))
  "Face for magenta colors in terminal"
  :group 'vterm)

(defface vterm-color-cyan
  '((t :foreground "#06989a"))
  "Face for cyan colors in terminal"
  :group 'vterm)

;; Orderless font faces
(defface orderless-match-face-0
  '((t :inherit 'font-lock-function-name-face))
  "First face for orderless matches")

(defface orderless-match-face-1
  '((t :inherit 'font-lock-keyword-face))
  "Second face for orderless matches")

(defface orderless-match-face-2
  '((t :inherit 'font-lock-variable-name-face))
  "Third face for orderless matches")

(defface orderless-match-face-3
  '((t :inherit 'font-lock-string-face))
  "Fourth face for orderless matches")

;; Vertico font faces
(defface vertico-current
  '((t :inherit region))
  "Face usedd to highlight the currently selected candidate")

;; Corfu font faces
(defface corfu-current
  '((t :inherit region))
  "Face used to highlight the currently selected candidate")

(defface corfu-default
  '((t :foreground "#cccccc" :background "#191919"))
  "Face used to highlight the currently selected candidate")

(defface corfu-border
  '((t :foreground "#333333" :background "#333333"))
  "Face used for thin border")

;; Autogenerated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6558fa279269fa352947bf310f7dc2b6e25745d539c9974628e569a3dbff78db" "68483b0eb8ad569b1212b1e160ff920ac98dc7cf67668a0eb53ffac632778300" "9b7cae793c4e8bbbdab743fa38f857176fb09f77ca0690d8aa960e40b3900109" "30af43ce54f038a9c1c60893a7100d40ef5e7e86d307a1b5c338f768708c32f9" "93bb13b355ca389a2929fc14bb27b8f864234ef3e444728871037d2d6e5c1299" "ee92cd6b037bbc6e496cdcca1e306c4c864ff6ee712c2fad33916a33b8bdb005" "52487739a5864f2c022ddf861c9c7cedad9c028ac61145a8a6447c6784ad4b9c" default))
 '(package-selected-packages
   '(vertico magit orderless git-gutter general evil which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
