(use-package magit)

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.50)
  :config
  (global-git-gutter-mode))

(provide 'git)
