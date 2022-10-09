;;; package --- Summary
;;; Absolutely blissful emacs configuration.
;;; Commentary:

;;; Code:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(set-face-attribute 'default nil
                    :family "InputMono Nerd Font"
                    :height 120
                    :weight 'regular)

(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabhsangpal@gmail.com")

;; General configuration.
(setq-default tab-width 8)
(setq-default tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

(blink-cursor-mode 0)
(setq column-number-mode t)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)
(global-auto-revert-mode t)

;; Load all plugins.
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(load "~/.config/emacs/plugins")
(require 'plugins)

(grep-apply-setting 'grep-template "rg --no-heading -H -uu -g <F> <R> <D>")

(provide 'init)
;;; init.el ends here
