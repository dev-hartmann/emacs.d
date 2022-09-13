(defun dh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org-contrib)

(use-package org
  :hook (org-mode .dh/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-agenda-files  '("~/.emacs.d/org/Tasks.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun dh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dh/org-mode-visual-fill))

(provide 'org)
