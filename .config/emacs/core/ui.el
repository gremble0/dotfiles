;; Set theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'yellowbeans t)

;; Set fonts
(set-face-attribute 'default nil
  :font "JetBrainsMono NFM"
  :height 140
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono NFM"
  :height 140
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Cantarell"
  :height 110
  :weight 'medium)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NFM-14"))

;; Colored emoji font
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(use-package which-key
  :config
  (which-key-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package rainbow-mode
  :custom
  (rainbow-r-colors nil)
  (rainbow-x-colors nil)
  (rainbow-html-colors nil)
  :hook
  org-mode prog-mode help-mode)

;; Apply ANSI color codes in compilation buffers
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'ui)
