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
  ;; (evil-set-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer)))

;;General keybindings
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (leader-keys
    "d" "dd"
    "y" "yy"
    "z" "zz"
    "bi" 'ibuffer
    "bb" 'switch-to-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "eb" 'eval-buffer))

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
;;(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

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
(setq display-line-numbers 'relative)
(global-visual-line-mode t)
