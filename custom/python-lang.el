;;; python.el

(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(use-package pip-requirements
  :after python)

(use-package pyenv-mode
  :after python
  :config
  (when (executable-find "pyenv")
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv")))))


                                        ;(use-package python-pytest
                                        ;  :commands python-pytest-dispatch
                                        ;:bind
                                        ;(:map python-mode-map
                                        ;      ("a" . python-pytest)
                                        ;      ("f" . python-pytest-file-dwim)
                                        ;      ("F" . python-pytest-file)
                                        ;      ("t" . python-pytest-function-dwim)
                                        ;      ("T" . python-pytest-function)
                                        ;      ("r" . python-pytest-repeat)
                                        ;      ("p" . python-pytest-dispatch)))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package auto-virtualenv
  :hook
  (python-mode . auto-virtualenv-set-virtualenv))

(use-package pyimport
  :after python)

(use-package py-isort
  :after python)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'python-lang)
;;; python.el ends here
