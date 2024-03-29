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

;; Load core modules
(use-package ui         :load-path "core")
(use-package editing    :load-path "core")
(use-package languages  :load-path "core")
(use-package navigation :load-path "core")
(use-package completion :load-path "core")

;; Set some sensible settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)

(setq-default display-line-numbers-type 'relative

              scroll-conservatively 101
              scroll-margin 5
              scroll-preserve-screen-position 't

              inhibit-startup-message t
              initial-scratch-message nil

              confirm-kill-processes nil
              kill-buffer-query-functions nil

              shell-file-name "/bin/zsh"
              compile-command ""
              
              backup-directory-alist '((".*" . "~/.cache/emacs"))
              
              tab-width 4
              indent-tabs-mode nil
              indent-line-function 'insert-tab

              undo-no-redo t

              help-window-select t)
