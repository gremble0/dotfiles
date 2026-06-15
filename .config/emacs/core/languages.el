;; LSP setup
(use-package eglot
  :hook
  ((c-mode c++-mode c-ts-mode c++-ts-mode
    go-mode go-ts-mode
    python-mode python-ts-mode
    java-mode java-ts-mode
    lua-mode lua-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

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
(setq eldoc-echo-area-use-multiline-p nil)

(provide 'languages)
