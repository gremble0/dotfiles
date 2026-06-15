(defun gremble/git-files ()
  "Find files tracked by git."
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

;; Dired
(setq dired-listing-switches "-AhgGoF --group-directories-first --color=auto"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(defadvice dired-up-directory (around dired-up-directory-alternate activate)
  "When going up a directory, kill the current dired buffer."
  (let ((current (current-buffer)))
    ad-do-it
    (kill-buffer current)))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(provide 'navigation)
