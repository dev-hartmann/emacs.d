;; Setup straight.el as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-cache-autoloads t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-profiles '((nil . "default.el")
                          (pinned . "pinned.el"))
      straight-repository-branch "develop"
      straight-use-package-by-default t
      use-package-always-ensure nil)

(use-package gcmh
  :demand
  :hook
  (focus-out-hook . gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600)
  :config
  (gcmh-mode +1))

(use-package no-littering
  :straight t)

(defun setup-load-path ()
  "Set up load path for the initialization process"
  (setq user-home-directory (getenv "HOME"))
  (setq user-customizations-directory (concat user-emacs-directory "custom/"))
  (add-to-list 'load-path user-customizations-directory))

;; start emacs as server to send files from cli/shell to this instance
(server-start)

(setup-load-path)

(require 'core)
(require 'editor)
(require 'ui)
(require 'settings)
(require 'search)
(require 'navigation)

(when (eq system-type 'darwin)
  (require 'macos))
(require 'util)
(require 'core-code)

(require 'clojure)
(require 'python-lang)
(require 'typescript)
(require 'devops)

(require 'transparency)
