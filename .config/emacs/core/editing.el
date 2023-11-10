;; Custom mode to stop other plugins from overwriting keybinds intended to be global
(defvar global-keys-map (make-keymap)
  "Keymap for global-keys-mode")

;; Automatically focus newly opened windows.
;; From: https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(setq split-window-preferred-function 'my/split-window-func)
(defun my/split-window-func (&optional window)
  (let ((new-window (split-window-sensibly window)))
    (if (not (active-minibuffer-window))
        (select-window new-window))))

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
  ;; Insert mode keybinds
  (evil-define-key 'insert global-keys-map
    (kbd "C-h") 'evil-backward-char
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line
    (kbd "C-l") 'evil-forward-char)

  ;; General keybinds
  (evil-define-key '(normal visual) global-keys-map
    (kbd "C-c C-i") 'ibuffer
    (kbd "C-c C-k") 'kill-buffer-and-window
    (kbd "C-c C-e") 'eval-buffer
    (kbd "C-c C-r") 'eval-region
    (kbd "C-c n")   'git-gutter:next-hunk
    (kbd "C-c p")   'git-gutter:previous-hunk
    (kbd "C-f")     'find-file
    (kbd "C-n")     'next-buffer
    (kbd "C-p")     'previous-buffer
    (kbd "C-e")     'compile
    (kbd "C-b")     'consult-buffer
    (kbd "C-s")     'consult-line
    (kbd "C-j")     'dired-jump
    (kbd "C-t")     'vterm-toggle)

  ;; Visual state keybinds
  (evil-define-key 'visual prog-mode-map
    (kbd "<tab>") 'indent-region
    (kbd "/")     'comment-or-uncomment-region)
    
  ;; Normal state keybinds
  (evil-define-key 'normal prog-mode-map
    (kbd "<tab>") 'evil-indent-line
    (kbd "/")     'comment-line)

  ;; Lsp keybinds
  (evil-define-key 'normal lsp-mode-map
    (kbd "K") 'lsp-ui-doc-show)

  ;; Dired keybinds
  (evil-define-key 'normal dired-mode-map
    (kbd "<backspace>") 'dired-single-up-directory
    (kbd "RET")         'dired-single-buffer
    (kbd "l")           'dired-single-buffer
    (kbd "h")           'dired-single-up-directory
    (kbd "C-t")         nil)

  (evil-define-key 'insert minibuffer-mode-map
    (kbd "C-p") 'previous-line-or-history-element
    (kbd "C-n") 'next-line-or-history-element)

  (evil-define-key 'normal compilation-mode-map
    (kbd "C-s") 'consult-compile-error)

  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package yasnippet
  :config
  (yas-global-mode))

(provide 'editing)
