;; Set theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'gremble-gruber t)

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
  (highlight-indent-guides-auto-character-face-perc 100)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package rainbow-mode
  :custom
  (rainbow-r-colors nil)
  (rainbow-x-colors nil)
  (rainbow-html-colors nil)
  :hook
  org-mode prog-mode help-mode)

(use-package magit)

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.50)
  (git-gutter:hide-gutter t)
  :config
  (global-git-gutter-mode))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-border "#282828")
  (lsp-ui-sideline-enable nil))

;; See font face for text at point
;; From: https://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Apply ANSI color codes in compilation buffers
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'ui)
