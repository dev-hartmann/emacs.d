;;; settings.el --- -*- lexical-binding: t; -*-
(setq-default indent-tabs-mode nil)

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq  tab-width 8
       require-final-newline t
       sentence-end-double-space nil
       save-place-file "~/.emacs.d/backups/saveplace"
       fringes-outside-margins t
       custom-file "~/.emacs.d/.custom.el"
       vc-follow-symlinks t
       version-control t
       kill-ring-max 120
       inhibit-eol-conversion t
       load-prefer-newer t
       confirm-kill-processes nil
       native-comp-async-report-warnings-errors 'silent
       truncate-string-ellipsis "…"
       executable-prefix-env t
       use-dialog-box nil
       delete-by-moving-to-trash t
       global-auto-revert-non-file-buffers t)

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
       read-process-output-max (* 1024 1024)
       sentence-end-double-space nil
       require-final-newline t
       kept-new-versions 10    ; keep 10 latest versions
       kept-old-versions 0     ; don't bother with old versions
       delete-old-versions t   ; don't ask about deleting old versions
       version-control t       ; number backups
       vc-make-backup-files t
       require-final-newline t
       read-process-output-max (* 1024 1024)
       auto-save-default nil
       create-lockfiles nil)

(provide 'settings)
;;; settings.el ends here
