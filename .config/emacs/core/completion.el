;; Vertico for completion framework
(use-package vertico
  :init
  ;; Mostly taken from the wiki: https://github.com/minad/vertico/wiki
  ;; Configure the default sorting function for symbols and files
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file
           (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))))

  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'dired-directory)
      file))
  :bind
  (:map minibuffer-local-map
        ("C-<backspace>" . (lambda (arg)
                             (interactive "p")
                             (if minibuffer-completing-file-name
                                 (if (string-match-p "/." (minibuffer-contents))
                                     (zap-up-to-char (- arg) ?/)
                                   (delete-minibuffer-contents))
                               (backward-kill-word arg)))))
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode))

(use-package consult
  :custom
  (consult-line-start-from-top t))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode))

(provide 'completion)
