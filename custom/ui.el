;;; ui.el --- -*- lexical-binding: t; -*-

(when window-system 
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
    (menu-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t))

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-height 25)
  (doom-modeline-indent-info nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode))

(provide 'ui)
;;; ui.el ends here
