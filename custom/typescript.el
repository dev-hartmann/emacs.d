;;; typescript.el
(use-package typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode))

(use-package rjsx-mode
  :config
  (setq js-chain-indent t
        ;; These have become standard in the JS community
        js2-basic-offset 2
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15))

(use-package web-mode)

(use-package js2-mode)

(provide 'typescript)
;;; typescript.el ends here
