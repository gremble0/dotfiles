;; Dired
(setq dired-listing-switches "-AhgGoF --group-directories-first --color=auto"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(defadvice dired-up-directory (around dired-up-directory-alternate activate)
  "When going up a directory, kill the current dired buffer."
  (let ((current (current-buffer)))
    ad-do-it
    (kill-buffer current)))

;; Terminal
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package vterm
  :custom
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project))

(provide 'navigation)
