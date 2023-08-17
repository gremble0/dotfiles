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
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-keys-general
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (leader-keys-general
    ;; Editing text
    "d" "dd"
    "y" "yy"
    "z" "zz"

    ;; Buffer shortcuts
    "bi" 'ibuffer
    "bb" 'switch-to-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bc" 'kill-this-buffer
    "be" 'eval-buffer

    ;; File navigation
    "ff" 'find-file
    "fr" 'counsel-recentf
    "fe" 'dired-jump
    "fs" 'dired

    ;; Splitting and closing windows
    "wc" 'evil-window-delete
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    ;; Navigating windows
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    ;; Moving windows
    "wH" 'evil-window-move-far-left
    "wJ" 'evil-window-move-very-bottom
    "wK" 'evil-window-move-very-top
    "wL" 'evil-window-move-far-right

    ;; Help
    "hf" 'describe-function
    "hv" 'describe-variable

    ;; Miscellaneous
    "g" 'magit-status
    "t" 'vterm-toggle)


  (general-create-definer leader-keys-visual
    :states '(visual)
    :keymaps 'override
    :prefix "SPC")
  
  (leader-keys-visual
    "/" 'comment-or-uncomment-region)

  (general-create-definer leader-keys-normal
    :states '(normal)
    :keymaps 'override
    :prefix "SPC")
  
  (leader-keys-normal
    "/" 'comment-line)

  (general-def 'insert
    "C-h" 'backward-char
    "C-j" 'next-line
    "C-k" 'previous-line
    "C-l" 'forward-char)

  ;; Dired keybinds
  (general-def ('normal 'insert) dired-mode-map
    "a" 'dired-create-empty-file
    "r" 'dired-do-rename
    "d" 'dired-do-delete
    "<return>" 'dired-open-file
    "<backspace>" 'dired-up-directory)

  (general-def minibuffer-local-completion-map
    "<return>" nil))

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

;; Command completion
(use-package ivy
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package counsel
  :after ivy
  :config (counsel-mode))

;; Icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package rainbow-mode
  :hook org-mode prog-mode)

(use-package magit)

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

;; Some keybinds
(keymap-global-set "C-c C-p" 'previous-buffer)
(keymap-global-set "C-c C-n" 'next-buffer)

;; Modeline
(setq-default mode-line-format
	      '("%e"
	        modeline-buffer-name
		"  "
		modeline-major-mode))

(defface modeline-default-face
  '((t :background "#191919" :foreground "#cccccc"))
  "Face for background color of modeline")

(defface modeline-buffer-name-style
  '((t :inherit modeline-default-face :foreground "#e1b655"))
  "Face for buffer-name section in modeline")

(defun modeline-buffer-name-format ()
  (format " %s " (buffer-name)))

(defvar-local modeline-buffer-name
    '(:eval
      (propertize (modeline-buffer-name-format) 'face 'modeline-buffer-name-style)))

(put 'modeline-buffer-name 'risky-local-variable t)

(defun modeline-major-mode-format ()
    (capitalize (symbol-name major-mode)))

(defvar-local modeline-major-mode
    '(:eval
      (propertize (modeline-major-mode-format) 'face 'modeline-default-face)))

(put 'modeline-major-mode 'risky-local-variable t)

;; Autogenerated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6558fa279269fa352947bf310f7dc2b6e25745d539c9974628e569a3dbff78db" "68483b0eb8ad569b1212b1e160ff920ac98dc7cf67668a0eb53ffac632778300" "9b7cae793c4e8bbbdab743fa38f857176fb09f77ca0690d8aa960e40b3900109" "30af43ce54f038a9c1c60893a7100d40ef5e7e86d307a1b5c338f768708c32f9" "93bb13b355ca389a2929fc14bb27b8f864234ef3e444728871037d2d6e5c1299" "ee92cd6b037bbc6e496cdcca1e306c4c864ff6ee712c2fad33916a33b8bdb005" "52487739a5864f2c022ddf861c9c7cedad9c028ac61145a8a6447c6784ad4b9c" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
