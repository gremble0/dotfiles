(defun gremble/rg-files-no-ignore ()
  "Find files from current directory using ripgrep, not respecting gitignore."
  (interactive)
  (let* ((cmd (concat "rg --files --no-ignore"))
         (files (split-string (shell-command-to-string cmd) "\n" t))
         (file (completing-read "Ripgrep: " files nil t)))
    (find-file (expand-file-name file default-directory))))

(defun gremble/build-nova ()
  (project-compile "bash -c 'source ./activate && build"))

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
