(deftheme gremble-gruber
  "grembles variant of the gruber-darker theme")

(let ((gremble-gruber-fg1       "#cccccc")
      (gremble-gruber-fg2       "#bcbcbc")
      (gremble-gruber-fg3       "#ababab")
      (gremble-gruber-fg4       "#9b9b9b")
      (gremble-gruber-bg1       "#151515")
      (gremble-gruber-bg2       "#191919")
      (gremble-gruber-bg3       "#1c1c1c")
      (gremble-gruber-bg4       "#282828")
      (gremble-gruber-bg5       "#3a3a3a")
      (gremble-gruber-bg6       "#606060")
      (gremble-gruber-gray      "#9ea4a2")
      (gremble-gruber-sky       "#9fb6c2")
      (gremble-gruber-niagara   "#96a6c8")
      (gremble-gruber-moss      "#7c9081")
      (gremble-gruber-olive     "#9aae6b")
      (gremble-gruber-yellow    "#e1b655")
      (gremble-gruber-good      "#60a840")
      (gremble-gruber-neutral   "#ffa500")
      (gremble-gruber-bad       "#d22b2b")
      (gremble-gruber-wisteria  "#9e95c7"))

  (custom-theme-set-variables
   'gremble-gruber
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'gremble-gruber

   ;; Main faces
   `(cursor                   ((t :background ,gremble-gruber-yellow)))
   `(default                  ((t :foreground ,gremble-gruber-fg1
                                  :background ,gremble-gruber-bg1)))
   `(fringe                   ((t :foreground ,gremble-gruber-fg4
                                  :background ,gremble-gruber-bg1))) ;; TODO
   `(help-key-binding         ((t :foreground ,gremble-gruber-yellow
                                  :background ,gremble-gruber-bg2
                                  :box (:color ,gremble-gruber-bg4))))
   `(highlight                ((t :background ,gremble-gruber-bg5)))
   `(line-number              ((t :foreground ,gremble-gruber-bg6)))
   `(line-number-current-line ((t :foreground ,gremble-gruber-fg1 :bold t)))
   `(link                     ((t :foreground ,gremble-gruber-niagara)))
   `(region                   ((t :background ,gremble-gruber-bg5)))
   `(vertical-border          ((t :foreground ,gremble-gruber-bg4)))
   `(show-paren-match         ((t :foreground ,gremble-gruber-yellow)))
   `(show-paren-mismatch      ((t :foreground ,gremble-gruber-bad)))
   `(minibuffer-prompt        ((t :foreground ,gremble-gruber-yellow)))
   `(isearch                  ((t :foreground ,gremble-gruber-yellow :background ,gremble-gruber-bg5)))
   `(trailing-whitespace      ((t :background ,gremble-gruber-bad)))

   ;; Fonts
   ;; `(fixed-pitch-serif ((t (:family JetBrainsMono Nerd Font))))
   ;; `(fixed-pitch ((t (:family JetBrainsMono Nerd Font))))

   ;; Mode line
   `(mode-line          ((t :foreground ,gremble-gruber-fg1
                            :background ,gremble-gruber-bg2
                            :box (:color ,gremble-gruber-bg4))))
   `(mode-line-inactive ((t :foreground ,gremble-gruber-fg3
                            :background ,gremble-gruber-bg4
                            :box (:color ,gremble-gruber-bg4))))

   ;; Statuses
   `(bad     ((t :foreground ,gremble-gruber-bad)))
   `(error   ((t :foreground ,gremble-gruber-bad)))
   `(success ((t :foreground ,gremble-gruber-good)))
   `(warning ((t :foreground ,gremble-gruber-neutral)))

   ;; Font lock
   `(font-lock-builtin-face         ((t :foreground ,gremble-gruber-yellow)))
   `(font-lock-comment-face         ((t :foreground ,gremble-gruber-bg6)))
   `(font-lock-constant-face        ((t :foreground ,gremble-gruber-moss)))
   `(font-lock-doc-face             ((t :foreground ,gremble-gruber-olive)))
   `(font-lock-doc-face-string-face ((t :foreground ,gremble-gruber-bad))) ;; TODO
   `(font-lock-function-name-face   ((t :foreground ,gremble-gruber-niagara)))
   `(font-lock-keyword-face         ((t :foreground ,gremble-gruber-yellow :bold t)))
   `(font-lock-negation-char-face   ((t :foreground ,gremble-gruber-bad))) ;; TODO
   `(font-lock-preprocessor-face    ((t :foreground ,gremble-gruber-moss)))
   `(font-lock-reference-face       ((t :foreground ,gremble-gruber-bad)))
   `(font-lock-string-face          ((t :foreground ,gremble-gruber-olive)))
   `(font-lock-type-face            ((t :foreground ,gremble-gruber-gray)))
   `(font-lock-variable-name-face   ((t :foreground ,gremble-gruber-niagara)))
   `(font-lock-warning-face         ((t :foreground ,gremble-gruber-neutral)))

   ;; Lsp mode
   `(lsp-ui-doc-background ((t :background ,gremble-gruber-bg2)))
   `(lsp-ui-peek-header    ((t :background ,gremble-gruber-bg2
                                            :foreground ,gremble-gruber-yellow)))
   `(lsp-ui-peek-highlight ((t :foreground ,gremble-gruber-yellow)))
   `(lsp-ui-peek-peek      ((t :background ,gremble-gruber-bg2)))
   `(lsp-ui-peek-selection ((t :background ,gremble-gruber-bg5)))

   ;; Dired
   `(dired-directory      ((t :foreground ,gremble-gruber-niagara)))
   `(dired-symlink        ((t :foreground ,gremble-gruber-moss)))
   `(dired-broken-symlink ((t :foreground ,gremble-gruber-bad)))

   ;; Terminal
   `(term               ((t ,(list :foreground gremble-gruber-fg1
                                   :background gremble-gruber-bg1))))
   `(term-color-black   ((t ,(list :foreground gremble-gruber-bg1
                                   :background gremble-gruber-bg1))))
   `(term-color-blue    ((t ,(list :foreground gremble-gruber-niagara
                                   :background gremble-gruber-niagara))))
   `(term-color-red     ((t ,(list :foreground gremble-gruber-bad
                                   :background gremble-gruber-bad))))
   `(term-color-green   ((t ,(list :foreground gremble-gruber-olive
                                   :background gremble-gruber-olive))))
   `(term-color-yellow  ((t ,(list :foreground gremble-gruber-yellow
                                   :background gremble-gruber-yellow))))
   `(term-color-magenta ((t ,(list :foreground gremble-gruber-wisteria
                                   :background gremble-gruber-wisteria))))
   `(term-color-cyan    ((t ,(list :foreground gremble-gruber-moss
                                   :background gremble-gruber-moss))))
   `(term-color-white   ((t ,(list :foreground gremble-gruber-fg1
                                   :background gremble-gruber-fg1))))

   ;; Git gutter
   `(git-gutter:added    ((t :foreground ,gremble-gruber-good)))
   `(git-gutter:deleted  ((t :foreground ,gremble-gruber-bad)))
   `(git-gutter:modified ((t :foreground ,gremble-gruber-neutral)))

   ;; Magit, TODO: revisit
   `(magit-item-highlight         ((t :background ,gremble-gruber-bg5)))
   `(magit-section-heading        ((t :foreground ,gremble-gruber-yellow :weight bold)))
   `(magit-hunk-heading           ((t :background ,gremble-gruber-bg5)))
   `(magit-section-highlight      ((t :background ,gremble-gruber-bg3)))
   `(magit-hunk-heading-highlight ((t :background ,gremble-gruber-bg5)))
   `(magit-diff-context-highlight ((t :background ,gremble-gruber-bg5 :foreground ,gremble-gruber-fg3)))
   `(magit-diffstat-added         ((t :foreground ,gremble-gruber-good)))
   `(magit-diffstat-removed       ((t :foreground ,gremble-gruber-bad)))
   `(magit-process-ok             ((t :foreground ,gremble-gruber-neutral :weight bold)))
   `(magit-process-ng             ((t :foreground ,gremble-gruber-bad :weight bold)))
   `(magit-branch                 ((t :foreground ,gremble-gruber-moss :weight bold)))
   `(magit-log-author             ((t :foreground ,gremble-gruber-fg3)))
   `(magit-hash                   ((t :foreground ,gremble-gruber-fg2)))
   `(magit-diff-file-header       ((t :foreground ,gremble-gruber-fg2 :background gremble-gruber-bg5)))

   ;; Orderless
   `(orderless-match-face-0 ((t :foreground ,gremble-gruber-yellow)))
   `(orderless-match-face-1 ((t :foreground ,gremble-gruber-moss)))
   `(orderless-match-face-2 ((t :foreground ,gremble-gruber-olive)))
   `(orderless-match-face-3 ((t :foreground ,gremble-gruber-wisteria)))

   ;; Vertico

   ;; Corfu
   `(corfu-current ((t :background ,gremble-gruber-bg5)))
   `(corfu-default ((t :background ,gremble-gruber-bg2)))
   `(corfu-border  ((t :background ,gremble-gruber-bg4)))

   ;; Highlight indent guides
   `(highlight-indent-guides-character-face ((t :foreground ,gremble-gruber-bg6)))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gremble-gruber)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gremble-gruber-theme.el ends here.
