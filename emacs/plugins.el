;;; package --- Summary:
;;; Load all the necessary plugins.

;;; Code:
;; Load straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setup use-package with straight.el.
(straight-use-package 'use-package)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setq evil-insert-state-cursor '(box "#1fa6de")
        evil-normal-state-cursor '(box "#111111")
        evil-visual-state-cursor '(box "#66ff66"))
  (evil-mode 1)
  :bind
  (:map evil-normal-state-map
        ("C-u" . 'evil-scroll-up)
        ("C-;" . #'evil-execute-in-god-state))
  )

(use-package evil-escape
  :after evil
  :straight t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence ",,")
  (setq-default evil-escape-delay 0.2))

(use-package god-mode
  :straight t
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil))

(use-package evil-god-state
  :straight t
  :config
  (evil-define-key 'normal global-map "C-;" 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

(use-package projectile
  :straight t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-SPC" . projectile-command-map))

(use-package company
  :straight t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2))

(use-package go-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package web-mode
  :straight t)

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))

(use-package hl-todo
  :straight t
  :init
  (global-hl-todo-mode 1)
  :config
  (setq hl-todo-keyword-faces
         '(("BUG" . "#ff3300")
           ("TODO" . "#00ace6")
           ("FIXME" . "#ff9900")
           ("NOTE" . "#339933"))))

(use-package magit
  :straight t)

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config (setq which-key-popup-type 'minibuffer))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package restart-emacs
  :straight t)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package solaire-mode
  :straight t
  :init
  (solaire-global-mode +1))

(load "~/.config/emacs/vertical-completion")
(require 'vertical-completion)

(provide 'plugins)
;;; Commentary:
;;; plugins.el ends here
