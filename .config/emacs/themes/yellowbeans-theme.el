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
      (good          "#b3e27c")
      (neutral       "#ffa500")
      (bad           "#d22b2b"))

  (custom-theme-set-faces
   'yellowbeans

   ;; Core
   `(default                      ((,class (:background ,bg1 :foreground ,fg1))))
   `(cursor                       ((,class (:background ,fg1))))
   `(fringe                       ((,class (:background ,bg1 :foreground ,fg3))))
   `(region                       ((,class (:background ,bg5))))
   `(highlight                    ((,class (:foreground ,fg3 :background ,bg5))))
   `(vertical-border              ((,class (:foreground ,bg4))))
   `(minibuffer-prompt            ((,class (:foreground ,gold-yellow :bold t))))
   `(trailing-whitespace          ((,class (:background ,bad))))
   `(hl-line                      ((,class (:background ,bg3))))

   ;; Font lock
   `(font-lock-comment-face       ((,class (:foreground ,bg6))))
   `(font-lock-doc-face           ((,class (:foreground ,bg6))))
   `(font-lock-constant-face      ((,class (:foreground ,moss-green))))
   `(font-lock-function-name-face ((,class (:foreground ,perano-blue))))
   `(font-lock-keyword-face       ((,class (:foreground ,gold-yellow :bold t))))
   `(font-lock-builtin-face       ((,class (:inherit font-lock-keyword-face))))
   `(font-lock-operator-face      ((,class (:inherit font-lock-keyword-face))))
   `(font-lock-string-face        ((,class (:foreground ,olive-green))))
   `(font-lock-type-face          ((,class (:foreground ,shipcove-blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face       ((,class (:foreground ,bad))))
   `(font-lock-punctuation-face   ((,class (:foreground ,hoki-blue))))

   ;; Status
   `(success                       ((,class (:foreground ,good))))
   `(warning                       ((,class (:foreground ,neutral))))
   `(error                         ((,class (:foreground ,bad))))

   ;; Mode line
   `(mode-line                     ((,class (:box (:color ,bg4) :foreground ,fg1 :background ,bg3))))
   `(mode-line-inactive            ((t (:inherit mode-line))))
   `(mode-line-buffer-id           ((,class (:foreground ,fg1))))

   ;; Line numbers
   `(line-number                   ((,class (:inherit fringe))))
   `(line-number-current-line      ((,class (:inherit fringe :foreground ,fg1 :weight bold))))

   ;; Paren matching
   `(show-paren-match              ((,class (:foreground ,gold-yellow))))
   `(show-paren-mismatch           ((,class (:foreground ,bad))))

   ;; Links
   `(link                          ((,class (:foreground ,shipcove-blue :underline t))))

   ;;; Plugins
   ;; Flymake
   `(flymake-warning               ((,class (:underline (:style wave :color ,neutral)))))
   `(flymake-error                 ((,class (:underline (:style wave :color ,bad)))))
   `(flymake-note                  ((,class (:underline (:style wave :color ,perano-blue)))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yellowbeans)
