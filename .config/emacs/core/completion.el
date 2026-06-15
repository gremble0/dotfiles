;; Vertico for completion framework
(use-package vertico
  :init
  (require 'vertico-multiform)
  (require 'vertico-sort)
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file
           (vertico-sort-function . gremble/vertico-sort-directories-first)
           (vertico-transform-functions . gremble/vertico-highlight-directory))))

  (defun gremble/vertico-sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defvar vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not vertico-transform-functions) null))
    (dolist (fun (ensure-list vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun gremble/vertico-highlight-directory (file)
    "If file ends with a slash, highlight it as a directory."
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
  (vertico-multiform-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package consult
  :custom
  (consult-async-split-style 'none))

(use-package embark)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode))

(use-package copilot)

(use-package copilot-chat)

(provide 'completion)
