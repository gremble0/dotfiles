(deftheme yellowbeans)

(let ((class '((class color) (min-colors 89)))
      (fg1           "#cccccc")
      (fg2           "#bcbcbc")
      (fg3           "#ababab")
      (bg1           "#151515")
      (bg3           "#1c1c1c")
      (bg4           "#282828")
      (bg5           "#333333")
      (bg6           "#606060")
      (shipcove-blue "#8197bf")
      (perano-blue   "#b0d0f0")
      (hoki-blue     "#526779")
      (dove-blue     "#a9b1d6")
      (gold-yellow   "#e1b655")
      (moss-green    "#7c9081")
      (olive-green   "#9aae6b")
      (var           "#9fb6c2")
      (good-fg       "#b3e27c")
      (good-bg       "#2e3127")
      (neutral-fg    "#ffa500")
      (neutral-bg    "#6b572a")
      (bad-fg        "#d22b2b")
      (bad-bg        "#503030"))

  (custom-theme-set-faces
   'yellowbeans

   ;; Core
   `(default                           ((,class (:background ,bg1 :foreground ,fg1))))
   `(cursor                            ((,class (:background ,fg1))))
   `(fringe                            ((,class (:background ,bg1 :foreground ,fg3))))
   `(region                            ((,class (:background ,bg5))))
   `(highlight                         ((,class (:foreground ,fg3 :background ,bg5))))
   `(vertical-border                   ((,class (:foreground ,bg4))))
   `(minibuffer-prompt                 ((,class (:foreground ,gold-yellow :bold t))))
   `(trailing-whitespace               ((,class (:background ,bad-fg))))
   `(hl-line                           ((,class (:background ,bg3))))

   ;; Isearch
   `(isearch                           ((,class (:background ,bg5))))
   `(isearch-fail                      ((,class (:background ,bad-fg))))
   `(isearch-group-1                   ((,class (:background ,neutral-fg))))
   `(isearch-group-2                   ((,class (:background ,good-fg))))

   ;; Links and help
   `(link                              ((,class (:foreground ,shipcove-blue :underline t))))
   `(help-key-binding                  ((,class (:foreground ,gold-yellow :box (:color ,bg4)))))
   `(escape-glyph                      ((,class (:inherit help-key-binding))))

   ;; Font lock
   `(font-lock-comment-face            ((,class (:foreground ,bg6))))
   `(font-lock-doc-face                ((,class (:foreground ,bg6))))
   `(font-lock-constant-face           ((,class (:foreground ,moss-green))))
   `(font-lock-function-name-face      ((,class (:foreground ,perano-blue))))
   `(font-lock-keyword-face            ((,class (:foreground ,gold-yellow :bold t))))
   `(font-lock-builtin-face            ((,class (:inherit font-lock-keyword-face))))
   `(font-lock-operator-face           ((,class (:inherit font-lock-keyword-face))))
   `(font-lock-string-face             ((,class (:foreground ,olive-green))))
   `(font-lock-type-face               ((,class (:foreground ,shipcove-blue))))
   `(font-lock-variable-name-face      ((,class (:foreground ,var))))
   `(font-lock-warning-face            ((,class (:foreground ,neutral-fg))))
   `(font-lock-punctuation-face        ((,class (:foreground ,hoki-blue))))

   ;; Status
   `(success                           ((,class (:foreground ,good-fg))))
   `(warning                           ((,class (:foreground ,neutral-fg))))
   `(error                             ((,class (:foreground ,bad-fg))))

   ;; Mode line
   `(mode-line                         ((,class (:box (:color ,bg4) :foreground ,fg1 :background ,bg3))))
   `(mode-line-inactive                ((,class (:inherit mode-line :foreground ,bg6))))
   `(minibuffer-prompt                 ((,class (:foreground ,gold-yellow :bold t))))

   ;; Tabs
   `(tab-bar                           ((,class (:inherit mode-line))))
   `(tab-bar-tab                       ((,class (:inherit mode-line))))
   `(tab-bar-tab-inactive              ((,class (:inherit mode-line-inactive))))

   ;; Line numbers
   `(line-number                       ((,class (:inherit fringe))))
   `(line-number-current-line          ((,class (:inherit fringe :foreground ,fg1 :weight bold))))

   ;; Paren matching
   `(show-paren-match                  ((,class (:foreground ,gold-yellow))))
   `(show-paren-mismatch               ((,class (:foreground ,bad-fg))))

   ;;; Plugins
   ;; Flymake
   `(flymake-warning                   ((,class (:underline (:style wave :color ,neutral-fg)))))
   `(flymake-error                     ((,class (:underline (:style wave :color ,bad-fg)))))
   `(flymake-note                      ((,class (:underline (:style wave :color ,perano-blue)))))

   ;; Orderless
   `(orderless-match-face-0            ((,class (:foreground ,gold-yellow))))
   `(orderless-match-face-1            ((,class (:foreground ,olive-green))))
   `(orderless-match-face-2            ((,class (:foreground ,shipcove-blue))))
   `(orderless-match-face-3            ((,class (:foreground ,moss-green))))

   ;; Whichkey
   `(which-key-key-face                ((,class (:foreground ,gold-yellow :bold t))))
   `(which-key-group-description-face  ((,class (:foreground ,shipcove-blue))))

   ;; Magit
   `(magit-branch-local                ((,class (:foreground ,shipcove-blue))))
   `(magit-branch-remote               ((,class (:foreground ,olive-green))))
   `(magit-diff-added                  ((,class (:foreground ,good-fg))))
   `(magit-diff-added-highlight        ((,class (:inherit (magit-section-highlight magit-diff-added)))))
   `(magit-diff-base                   ((,class (:foreground ,neutral-fg magit-diff-base))))
   `(magit-diff-base-highlight         ((,class (:inherit (magit-section-highlight )))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg6))))
   `(magit-diff-removed                ((,class (:foreground ,bad-fg))))
   `(magit-diff-removed-highlight      ((,class (:inherit (magit-section-highlight magit-diff-removed)))))
   `(magit-section-heading             ((,class (:foreground ,gold-yellow :bold t))))
   `(magit-section-highlight           ((,class (:background ,bg5))))

   ;; Git gutter
   `(git-gutter:added                  ((,class (:foreground ,good-fg))))
   `(git-gutter:modified               ((,class (:foreground ,neutral-fg))))
   `(git-gutter:deleted                ((,class (:foreground ,bad-fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yellowbeans)
