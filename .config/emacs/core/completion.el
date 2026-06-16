;; Vertico for completion framework
(use-package vertico
  :init
  (defun gremble/vertico-highlight-directory (file)
    (if (string-suffix-p "/" file)
        (propertize file 'face 'dired-directory)
      file))

  (defvar vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not vertico-transform-functions) null))
    (dolist (fun (ensure-list vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (setq vertico-sort-function
        (lambda (files)
          (let ((sorted (vertico-sort-alpha files)))
            (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) sorted)
                   (seq-remove (lambda (x) (string-suffix-p "/" x)) sorted))))
        vertico-multiform-categories
        '((file (vertico-transform-functions . gremble/vertico-highlight-directory))))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package consult)

(use-package embark)

(use-package embark-consult)

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
