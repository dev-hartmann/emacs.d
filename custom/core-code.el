;;; core-code.el


;; Pop up a shell everywhere
(use-package shell-pop
  :bind (("C-x t" . shell-pop)
         ("C-x c" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh"))

;; Tree-sitter setup
(use-package tree-sitter
  :straight t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;; LSP
(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  (lsp-enable-which-key-integration t)
  :hook ((java-mode . lsp)
         (clojure-mode . lsp)
         (javascript-mode . lsp)
         (typescript-mode . lsp)
         (typescript-tsx-mode .lsp)
         (rjsx-mode . lsp)
         (web-mode . lsp)
         (js2-mode . lsp)
         (python-mode .lsp))
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :custom
  (lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :config (dap-auto-configure-mode))

;; Completion via company
(use-package company
  :straight t
  :diminish
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  :custom
  (company-show-numbers t)
  (company-tooltip-idle-delay 0.25 "Faster!")
  (company-async-timeout 20 "Some requests take longer"))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :straight t
  :config (company-quickhelp-mode))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package package-lint
  :defer t)

(use-package package-build
  :defer t)

(use-package flycheck-package
  :defer t
  :config
  (flycheck-package-setup))

(provide 'core-code)
;;; core-code.el ends here
