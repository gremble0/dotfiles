(use-package lua-mode)
(use-package go-mode)

(use-package geiser
  :custom
  (geiser-active-implementations '(racket)))

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
  (lsp-completion-mode . corfu-setup-completion)
  (c-mode . lsp)
  (go-mode . lsp))

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

(provide 'languages)
