;;; init.el --- Main entrypoint for my personal emacs configuration

;;; Commentary:
;; This file is the main entry point to my emacs config that initializes
;; the package manager and loads all the modules under ./core. It also
;; changes some default variables

;;; Code:
;; Initialize package manager
(require 'package)
(require 'use-package-ensure)

(setq package-user-dir "~/.config/emacs/packages"
      use-package-always-ensure t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq-default
 indent-tabs-mode nil
 scroll-margin 5
 tab-width 4
 display-line-numbers-type 'relative)

(setq
 backup-directory-alist '((".*" . "~/.cache/emacs"))
 compile-command ""
 confirm-kill-processes nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 help-window-select t
 indent-line-function 'insert-tab
 inhibit-startup-message t
 initial-scratch-message nil
 kill-buffer-query-functions nil
 scroll-conservatively 101
 scroll-preserve-screen-position 't
 shell-file-name "/bin/zsh"
 undo-no-redo t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

;; Load core modules
(use-package ui         :load-path "core")
(use-package mappings   :load-path "core")
(use-package languages  :load-path "core")
(use-package navigation :load-path "core")
(use-package completion :load-path "core")
(use-package git        :load-path "core")
