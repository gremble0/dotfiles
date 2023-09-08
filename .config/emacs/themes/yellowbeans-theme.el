;; Theme template made with https://mswift42.github.io/themecreator/
;; based on my previous neovim scheme that you can find here https://github.com/gremble0/yellowbeans.nvim
(deftheme yellowbeans)
(let ((class '((class color) (min-colors 89)))
      (fg1 "#cccccc")
      (fg2 "#bcbcbc")
      (fg3 "#ababab")
      (fg4 "#9b9b9b")
      (fg6 "#d8d8d8")
      (bg1 "#151515")
      (bg2 "#191919")
      (bg3 "#1c1c1c")
      (bg4 "#3a3a3a")
      (bg5 "#4d4d4d")
      (builtin   "#e1b655")
      (keyword   "#b0d0f0")
      (directory "#8197bf")
      (link      "#6197af")
      (const     "#cf6a4c")
      (comment   "#606060")
      (func      "#e1b655")
      (str       "#9aae6b")
      (type      "#ffb964")
      (var       "#b6a6ff")
      (selection "#333333")
      (good      "#60a840")
      (neutral   "#ffa500")
      (bad       "#d22b2b")
      (unspec   (when (>= emacs-major-version 29) 'unspecified)))
  (custom-theme-set-faces
   'yellowbeans
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,bad :background ,bg3))))
   `(font-lock-warning-face ((,class (:foreground ,bad :background ,bg3))))
   `(xref-line-number ((,class (:inherit line-number))))
   `(xref-match ((,class (:inherit orderless-match-face-0))))
   `(xref-file-header ((,class (:height 1.1 :foreground ,builtin))))
   `(tree-sitter-hl-face:property ((,class (:inherit font-lock-property-name-face))))
   `(tree-sitter-hl-face:attribute ((,class (:inherit font-lock-property-name-face))))
   `(tree-sitter-hl-face:label ((,class (:inherit font-lock-property-name-face))))
   `(tree-sitter-hl-face:function.call ((,class (:inherit font-lock-function-call-face))))
   `(lsp-ui-doc-background ((,class (:background ,bg2))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg4))))
   `(hl-line ((,class (:background  ,bg3))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
   `(cursor ((,class (:background ,fg1))))
   `(isearch ((,class (:bold t :foreground ,bad :background ,bg4))))
   `(mode-line ((,class (:box (:line-width 4 :color ,bg2) :bold t :foreground ,fg1 :background ,bg2))))
   `(mode-line-inactive ((t (:inherit mode-line))))
   `(mode-line-buffer-id ((,class (:box nil :bold t :foreground ,fg1 :background ,unspec))))
   `(mode-line-highlight ((,class (:box nil :foreground ,selection :weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(vertical-border ((,class (:foreground ,selection))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,builtin))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:bold t :foreground ,builtin :height 1.1))))
   `(org-level-2 ((,class (:bold nil :foreground ,str))))
   `(org-level-3 ((,class (:bold t :foreground ,const))))
   `(org-level-4 ((,class (:bold nil :foreground ,bg5))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-block ((,class (:foreground ,fg2))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg4) :bold t :foreground ,bg5))))
   `(org-bad ((,class (:underline t :foreground ,bad))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg4))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
   `(org-agenda-done ((,class (:foreground ,bg5))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(ido-only-match ((,class (:foreground ,bad))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ivy-current-match ((,class (:foreground ,fg3 :inherit highlight :underline t))))
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(ffap ((,class (:foreground ,fg4))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   `(js3-bad-face ((,class (:underline ,keyword))))
   `(js3-error-face ((,class (:underline ,bad))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,fg2))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(bad ((,class (:foreground ,bad))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground ,unspec :background ,bad)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
   `(magit-item-highlight ((,class :background ,bg4)))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-hunk-heading           ((,class (:background ,bg4))))
   `(magit-section-highlight      ((,class (:background ,bg3))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg4))))
   `(magit-diff-context-highlight ((,class (:background ,bg4 :foreground ,fg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,bad :weight bold))))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg4))))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg4))))
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg1 :background ,bg1))))
   `(term-color-red ((,class (:foreground ,bad :background ,bad))))
   `(term-color-green ((,class (:foreground ,str :background ,str))))
   `(term-color-yellow ((,class (:foreground ,func :background ,func))))
   `(term-color-blue ((,class (:foreground ,directory :background ,directory))))
   `(term-color-magenta ((,class (:foreground ,var :background ,var))))
   `(term-color-cyan ((,class (:foreground ,link :background ,link))))
   `(term-color-white ((,class (:foreground ,fg1 :background ,fg1))))
   `(dired-directory ((,class (:foreground ,directory))))
   `(dired-symlink ((,class (:foreground ,link))))
   `(dired-broken-symlink ((,class (:foreground ,bad))))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,bad)))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-bad-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))
   `(yas-field-highlight-face ((t (:background ,selection))))
   `(git-gutter:added ((t :inherit default :weight bold :foreground ,good)))
   `(git-gutter:modified ((t :inherit git-gutter:added :foreground ,neutral)))
   `(git-gutter:modified ((t :inherit git-gutter:added :foreground ,bad)))
   `(orderless-match-face-0 ((t :inherit 'font-lock-function-name-face)))
   `(orderless-match-face-1 ((t :inherit 'font-lock-keyword-face)))
   `(orderless-match-face-2 ((t :inherit 'font-lock-variable-name-face)))
   `(orderless-match-face-3 ((t :inherit 'font-lock-string-face)))
   `(vertico-current ((t :inherit region)))
   `(corfu-current ((t :inherit region)))
   `(corfu-default ((t :background ,bg2)))
   `(corfu-border ((t :foreground ,selection :background ,selection)))
   `(highlight-indent-guides-character-face ((t :foreground ,comment))))
   ;; Legacy
   (if (< emacs-major-version 22)
       (custom-theme-set-faces
        'yellowbeans
        `(show-paren-match-face ((,class (:background ,builtin))))) ;; obsoleted in 22.1, removed 2016
     (custom-theme-set-faces
      'yellowbeans
      `(show-paren-match ((,class (:foreground ,builtin))))
      `(show-paren-mismatch ((,class (:foreground ,bad))))))
   ;; emacs >= 26.1
   (when (>= emacs-major-version 26)
     (custom-theme-set-faces
      'yellowbeans
      `(line-number ((,class (:foreground ,comment))))
      `(line-number-current-line ((,class (:foreground ,fg1 :weight bold))))))

  ;; emacs >= 27.1
  (when (>= emacs-major-version 27)
    (custom-theme-set-faces
     'yellowbeans
     `(tab-line              ((,class (:background ,bg3 :foreground ,fg4))))
     `(tab-line-tab          ((,class (:inherit tab-line))))
     `(tab-line-tab-inactive ((,class (:background ,bg3 :foreground ,fg4))))
     `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg1))))
     `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg2))))))
 (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'yellowbeans
     `(line-number ((t (:inherit fringe))))
     `(line-number-current-line ((t (:inherit fringe :foreground ,fg6 :weight bold))))))
;; emacs >= 27.1
(when (>= emacs-major-version 27)
  (custom-theme-set-faces
   'yellowbeans
   `(tab-line              ((,class (:background ,bg3 :foreground ,fg4))))
   `(tab-line-tab          ((,class (:inherit tab-line))))
   `(tab-line-tab-inactive ((,class (:background ,bg3 :foreground ,fg4))))
   `(tab-line-tab-current  ((,class (:background ,bg1 :foreground ,fg1))))
   `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg2))))))
 (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'yellowbeans
     `(tab-line-tab-modified ((,class (:foreground ,bad :weight bold))))))
  (when (boundp 'font-lock-regexp-face)
    (custom-theme-set-faces
    'yellowbeans
    `(font-lock-regexp-face ((,class (:inherit font-lock-string-face :underline t)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yellowbeans)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; yellowbeans-theme.el ends here
