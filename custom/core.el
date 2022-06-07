;;; core.el --- -*- lexical-binding: t; -*-
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(column-number-mode)
(line-number-mode t)
(size-indication-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(savehist-mode t)
(recentf-mode t)
(save-place-mode t)

(use-package auto-package-update
  :straight t
  :commands update-packages
  :custom
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "TMPDIR"
                                    "KUBECONFIG"
                                    "GOPATH"
                                    "GOBIN"
                                    "GOROOT"
                                    "GOPRIVATE"
                                    "GOENV_GOPATH_PREFIX"
                                    "GOENV_VERSION"))
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-debug nil)
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :straight t
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  (which-key-show-major-mode))

(use-package consult
  :demand t
  :bind (("C-t" . siren-consult-imenu)
         ("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         ("C-c b b" . consult-buffer)
         ("C-c C-p" . consult-projectile)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :preface
  (defun siren-consult-imenu ()
    "Intelligently trigger consult-lsp-file-symbols or consult-imenu."
    (interactive)
    (if (and (fboundp 'consult-lsp-file-symbols)
             (boundp 'lsp-mode)
             lsp-mode)
        ;; consult-lsp-file-symbols errors on some language servers, in such
        ;; a case, fall back to consult-imenu.
        (condition-case _
            (consult-lsp-file-symbols)
          ('error (consult-imenu)))
      (consult-imenu)))
  :custom
  (consult-project-root-function #'dh/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package vertico
  :custom
  (Vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("M-RET" . crux-smart-open-line-above)
         ("S-RET" . crux-smart-open-line)
         ("C-c e" . crux-eval-and-replace)
         ("C-c b c" . crux-cleanup-buffer-or-region)))

(add-hook 'before-save-hook #'crux-cleanup-buffer-or-region)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(provide 'core)
;;; core.el ends here
