;;; custom.el --- Summary
;;; Just a set of customizations that I don't know where else to put.
;;; Code:

(defun go-light-mode ()
    "Activate light mode and the customizations that go with it."
  (interactive)
  (load-theme 'modus-operandi t)
  (setq evil-normal-state-cursor '(box "#111111")))

(defun go-dark-mode ()
    "Activate dark mode and the customizations that go with it."
  (interactive)
  (load-theme 'modus-vivendi t)
  (setq evil-normal-state-cursor '(box "#eeeeee")))

(provide 'custom)
;;; Commentary:
;;; custom.el ends here
