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
  (c-mode . lsp)
  (c++-mode . lsp)
  (go-mode . lsp)
  (python-mode . lsp)
  (java-mode . lsp)
  (scheme-mode . lsp-scheme))

(use-package lsp-pyright)
(use-package lsp-java)

;; Treesitter for better syntax highlighting
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; Language specific settings
(setq-default c-basic-offset 4)

(provide 'languages)
