;;; init.el --- Summary
;;; Absolutely blissful emacs configuration.

;;; Code:
(set-face-attribute 'default nil
                    :family "InputMono Nerd Font"
                    :height 120
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :family "InputSans Nerd Font"
                    :height 1.0)

;; General configuration.
(setq-default tab-width 8
	      tab-stop-list (number-sequence 4 120 4)
	      indent-tabs-mode nil
	      message-log-max nil)

(kill-buffer "*Messages*")
(setq ring-bell-function 'ignore
      inhibit-splash-screen t
      inhibit-startup-screen t
      initial-buffer-choice nil
      column-number-mode t
      make-backup-files nil
      enable-recursive-minibuffers t
      read-extended-command-predicate #'command-completion-default-include-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)
(global-auto-revert-mode t)

;; Load all plugins.
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(global-set-key (kbd "C-c d") 'delete-window)
(global-set-key (kbd "C-c D") 'delete-other-windows)

(setq modus-themes-bold-constructs nil
      modus-themes-italic-constructs t
      modus-themes-syntax '(green-strings yellow-comments)
      modus-themes-links '(neutral-underline faint italic)
      modus-themes-box-buttons '(variable-pitch flat)
      modus-themes-prompts '(intense bold)
      modus-themes-mode-line '(accented borderless)
      modus-themes-tabs-accented t
      modus-themes-completions '((matches . (bold background intense))
                                 (selection . (semibold accented intense))
                                 (popup . (accented)))
      modus-themes-fringes nil
      modus-themes-lang-checkers '(intense background)
      modus-themes-hl-line '(accented underline)
      modus-themes-subtle-line-numbers t
      modus-themes-intense-mouseovers t
      modus-themes-paren-match '(underline intense)
      modus-themes-region '(bg-only accented)
      modus-themes-variable-pitch-ui t)

(load-theme 'modus-operandi t)

(load "~/.config/emacs/plugins")
(require 'plugins)

(load "~/.config/emacs/custom")
(require 'custom)

(grep-apply-setting 'grep-template "rg --no-heading -H -uu -g <F> <R> <D>")

(provide 'init)
;;; Commentary:
;;; init.el ends here
