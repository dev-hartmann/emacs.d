;;; macos.el --- -*- lexical-binding: t; -*-

;; Enable transparent titlebar
(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode))

;; Enable use of macOS trash
(use-package osx-trash
  :custom
  (delete-by-moving-to-trash t)
  :config
  (osx-trash-setup))

;; osx keyboard-fixes
(when (eq system-type 'darwin)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

;; Don't use macOS' Native fullscreen mode.
(setq ns-use-native-fullscreen nil)

(provide 'macos)
;;; macos.el ends here
