;;; python.el
(use-package python-mode)

(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package pip-requirements
  :after python)

(use-package pyenv-mode
  :after python
  :config
  (when (executable-find "pyenv")
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv")))))


(use-package python-pytest
  :commands python-pytest-dispatch
  :bind
  (:map python-mode-map
    ("a" . python-pytest)
    ("f" . python-pytest-file-dwim)
    ("F" . python-pytest-file)
    ("t" . python-pytest-function-dwim)
    ("T" . python-pytest-function)
    ("r" . python-pytest-repeat)
    ("p" . python-pytest-dispatch)))

(use-package pyimport
  :after python)

(use-package py-isort
   :after python)

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(provide 'python-lang)
;;; python.el ends here
