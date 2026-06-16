(defun gremble/rg (&optional rg-args)
  "Find files from current directory using ripgrep.
`rg-args' is a string of additional arguments to pass to rg."
  (interactive)
  (let* ((cmd (concat "rg " (or rg-args "")))
         (files (split-string (shell-command-to-string cmd) "\n" t))
         (file (completing-read "Ripgrep: " files nil t)))
    (find-file (expand-file-name file default-directory))))

(defun gremble/rg-files-no-ignore ()
  "Find files from current directory using ripgrep, not respecting gitignore."
  (interactive)
  (gremble/rg "--files --no-ignore"))

(defun gremble/rg-files ()
  "Find files from current directory using ripgrep."
  (interactive)
  (gremble/rg "--files"))

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
