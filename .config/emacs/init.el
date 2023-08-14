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

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Install packages
;; Evil mode
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-below t)
  (setq evil-want-fine-undo 'fine)
;;  (evil-set-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer)))

;; General keybindings
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (leader-keys
    ;; Leader combinations instead of double tapping key
    "d" "dd"
    "y" "yy"
    "z" "zz"

    ;; Toggle terminal
    "t" 'vterm-toggle

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
    "wH" 'buf-move-left
    "wJ" 'buf-move-down
    "wK" 'buf-move-up
    "wL" 'buf-move-right))

;; Terminal
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
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

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

;; Set theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'yellowbeans)

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
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("563cc3ad0df78ac9691c150254f1084b0334aedf33dc01b566bd33a22a9eb77f" "33f4fa35c268d2779bb05211071a75dfd12152d1f46c2ded86a15b5514e40271" "f141951372d24cbdb7469ed9fc43bcdb3691838de40d26ea950f49ebd18ab85a" "4dd33f2795b154b2a21a6079285ae8d4aab653f5f3cc0e2b857358d782e57b62" "41ef9af501b68a2474672089d2ba818674b34058cada543fedf265467cc4c202" "d66e73935c8d9fed33070c398b505b8cd8554f80b3722690a18990a028604b5d" "a468973b4a27e240676ab020b544a21f36fd8f21ae81112dad17fd3b4c1aed42" "571fddb8ecce64c9d85a0cac276722db95760366c23d2c915fda18e000a3ffa3" "95a0ce102e59f5e815fd66f11272ea36d8729a205161c7d5e82f292e5a5ac2d8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
