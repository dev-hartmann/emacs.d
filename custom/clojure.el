;;; cljojure.el
(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :hook (clojure-mode . lsp-deferred)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, w
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

;;(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(use-package clojure-snippets
  :straight t)

(use-package cider
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli)
  :custom ((cider-eldoc-display-context-dependent-info t)
           (cider-repl-pop-to-buffer-on-connect 'display-only)
           (cider-overlays-use-font-lock t)
           (cider-repl-buffer-size-limit 60000)))

(use-package flycheck-clj-kondo)

(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clojure-mode-extra-font-locking)
(setq cider-show-error-buffer t
      cider-auto-select-error-buffer t
      cider-repl-wrap-history t)

(provide 'clojure)
;;; clojure.el ends here
