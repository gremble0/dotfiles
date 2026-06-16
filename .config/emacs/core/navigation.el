(defun gremble/rg (path &optional rg-args)
  "Find files from PATH using ripgrep.
`rg-args' is a string of additional arguments to pass to rg."
  (interactive)
  (let* ((default-directory (expand-file-name path))
         (cmd (concat "rg " (or rg-args "")))
         (files (split-string (shell-command-to-string cmd) "\n" t))
         (file (completing-read "Ripgrep: " files nil t)))
    (find-file (expand-file-name file default-directory))))

(defun gremble/rg-files-no-ignore ()
  "Find files from current directory using ripgrep, not respecting gitignore."
  (interactive)
  (gremble/rg default-directory "--files --no-ignore"))

(defun gremble/rg-files ()
  "Find files from git root using ripgrep."
  (interactive)
  (let ((git-root (locate-dominating-file default-directory ".git")))
    (if git-root
        (gremble/rg git-root "--files")
      (user-error "Not inside a git repository"))))

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
