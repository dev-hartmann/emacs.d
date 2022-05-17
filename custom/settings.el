;;; settings.el --- -*- lexical-binding: t; -*-
(setq inhibit-startup-message t
      visible-bell nil)

;; Backups enabled, use nil to disable
(setq  backup-by-copying t         ;; Don't delink hardlinks
       backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
       bookmark-default-file "~/.emacs.d/bookmarks"
       custom-safe-themes t        ;; I may as well trust themes.
       create-lockfiles nil        ;; Don't create lock files.
       delete-old-versions t       ;; Automatically delete excess backups
       echo-key-strokes 0.2
       idle-update-delay 1.1
       version-control t      ; Use version numbers on backups
       delete-old-versions t  ; Automatically delete excess backups
       kept-new-versions 20   ; how many of the newest versions to keep
       kept-old-versions 5
       read-process-output-max (* 1024 1024)
       sentence-end-double-space nil
       require-final-newline t)

(provide 'settings)
;;; settings.el ends here
