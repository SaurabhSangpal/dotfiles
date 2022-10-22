;;; package --- Summary:
;;; Load all the necessary plugins.
;;; Commentary:

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

(defun scroll-down-5x()
  "Scrolls down 5 lines."
  (interactive)
  (scroll-up-line 5))

(defun scroll-up-5x()
  "Scrolls up 5 lines."
  (interactive)
  (scroll-down-line 5))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setq evil-insert-state-cursor '(box "#1fa6de")
        evil-normal-state-cursor '(box "#eeeeee")
        evil-visual-state-cursor '(box "#66ff66"))
  (evil-mode 1)
  :bind
  (:map evil-normal-state-map
        ("C-e" . 'scroll-down-5x)
        ("C-y" . 'scroll-up-5x)
        ("C-u" . 'evil-scroll-up)
        ("C-;" . #'evil-execute-in-god-state))
  )

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

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

(use-package helm
  :straight t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project)
  (global-set-key (kbd "C-<tab>") 'helm-dabbrev)
  (setq completion-styles '(flex))
  (helm-mode 1))

(use-package helm-ls-git
  :straight t)

(use-package helm-descbinds
  :straight t
  :init
  (helm-descbinds-mode))

(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package helm-company
  :straight t
  :config
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company))))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

(use-package projectile
  :straight t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-SPC" . projectile-command-map))

(use-package helm-projectile
  :straight t
  :hook (after-init . helm-projectile-on))

(use-package tree-sitter
  :straight t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :config
  (add-hook 'prog-mode-hook #'tree-sitter-hl-mode))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "s-l")
  :config
  (setq lsp-enable-file-watchers nil)
  (setq lsp-log-io nil)
  (setq format-with-lsp nil)
   :hook (
         (gdscript-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package dap-mode
  :straight t
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

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
         '(("BUG" . "#ff9999")
           ("TODO" . "#00ace6")
           ("FIXME" . "#ff9900")
           ("NOTE" . "#66ff66"))))

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

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil))
;;  (load-theme 'doom-flatwhite t))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package restart-emacs
  :straight t)

(provide 'plugins)
;;; plugins.el ends here
