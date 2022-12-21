;;; early-init --- Summary
;;; Some settings.
;;; Code:
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(setq package-enable-at-startup nil)

(blink-cursor-mode 0)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;; Commentary:
;;; early-init.el ends here
