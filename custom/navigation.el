;;; navigation.el --- -*- lexical-binding: t; -*-
(use-package anzu
  :straight t
  :config
  (global-anzu-mode t))

(use-package avy
  :straight t
  :bind ("M-s" . avy-goto-char))

(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m d" . #'mc/mark-all-dwim)))

(use-package windmove
  :config
  (windmove-default-keybindings 'super)
  (setq windmove-wrap-around t))

(provide 'navigation)
;;; navigation.el ends here
