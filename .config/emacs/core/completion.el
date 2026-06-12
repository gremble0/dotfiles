;; Vertico for completion framework
(use-package vertico
  :init
  ;; Mostly taken from the wiki: https://github.com/minad/vertico/wiki
  ;; Configure the default sorting function for symbols and files
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

  (defun gremble/vertico-git-files ()
    "Find files tracked by git, like :Telescope git_files in Neovim."
    (interactive)
    (let* ((root (locate-dominating-file default-directory ".git"))
           (files (if root
                      (let ((default-directory root))
                        (split-string
                         (shell-command-to-string "git ls-files --cached --others --exclude-standard")
                         "\n" t))
                    (user-error "Not inside a git repository")))
           (table (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . file))
                      (complete-with-action action files str pred))))
           (file (let ((default-directory root))
                   (completing-read "Git files: " table nil t))))
      (find-file (expand-file-name file root))))
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

(use-package marginalia
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
