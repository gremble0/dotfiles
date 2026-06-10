;; Major mode for languages
(use-package lua-mode)
(use-package go-mode)

;; LSP setup
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
  (lsp-completion-mode . corfu-setup-completion)
  :config
  ;; Streamlined loop hooks lsp into both classic and tree-sitter major modes
  (dolist (lang '(c c++ go python java lua))
    (let ((classic-mode (intern (concat (symbol-name lang) "-mode")))
          (ts-mode (intern (concat (symbol-name lang) "-ts-mode"))))
      (add-hook (intern (concat (symbol-name classic-mode) "-hook")) #'lsp)
      (add-hook (intern (concat (symbol-name ts-mode) "-hook")) #'lsp))))

(use-package lsp-pyright)
(use-package lsp-java)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) 
  (global-treesit-auto-mode)
  (setq treesit-font-lock-level 4))

;; Language specific settings
(setq-default c-basic-offset 4)

(provide 'languages)
