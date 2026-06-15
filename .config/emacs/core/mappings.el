;; Custom mode to stop other plugins from overwriting keybinds intended to be global
(defvar global-keys-map (make-keymap)
  "Keymap for global-keys-mode")

(define-minor-mode global-keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap global-keys-map)

(add-to-list 'emulation-mode-map-alists
			 `((global-keys-mode . ,global-keys-map)))

;; Editing
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo 'fine)
  (evil-undo-system 'undo-redo)
  :config
  ;; General keybinds
  (evil-define-key 'normal global-keys-map
	(kbd "C-x b") 'consult-buffer
	(kbd "C-c C-e") 'eval-buffer
    (kbd "C-c C-f") 'gremble/git-files
    (kbd "C-c C-g") 'consult-ripgrep
	(kbd "C-c C-l") 'magit-blame
    ;; These two are actually already bound by evil-collection, but they get unbound
    ;; in some buffer types for some reason so just make them truly global
	(kbd "[ b") 'evil-prev-buffer
	(kbd "] b") 'evil-next-buffer
	(kbd "[ g") 'git-gutter:previous-hunk
	(kbd "] g") 'git-gutter:next-hunk
	(kbd "[ q") 'previous-error
	(kbd "] q") 'next-error)

  ;; Visual state keybinds
  (evil-define-key 'visual prog-mode-map
    (kbd "<tab>") 'indent-region
    (kbd "/") 'comment-or-uncomment-region
	(kbd "C-c C-e") 'eval-region)

  ;; Normal state keybinds
  (evil-define-key 'normal prog-mode-map
	(kbd "<tab>") 'evil-indent-line)

  ;; Dired keybinds
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") 'dired-find-alternate-file)

  (evil-define-key '(insert normal) minibuffer-mode-map
    (kbd "C-p") 'previous-line-or-history-element
    (kbd "C-n") 'next-line-or-history-element
    (kbd "C-q") 'embark-export)

  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(provide 'mappings)
