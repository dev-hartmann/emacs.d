;;;

(use-package anzu
  :straight t
  :config
  (global-anzu-mode t))

(use-package avy
  :straight t
  :bind ("M-s" . avy-goto-char))

(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)))

(provide 'navigation)
;;;
