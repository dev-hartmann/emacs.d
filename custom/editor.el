;;; editor.el --- -*- lexical-binding: t; -*-
(use-package imenu
  :straight (:type built-in)
  :defer t
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 160)
  (imenu-max-items 400))

;; All the icons config
(use-package memoize)

(use-package all-the-icons
  :after memoize
  :if window-system)

(use-package all-the-icons-ibuffer
  :after all-the-icons
  :if window-system
  :init
  (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :if window-system
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)

  :config
  ;; Override default category lookup function.
  (defun all-the-icons-completion-get-icon (cand cat)
    "Return the icon for the candidate CAND of completion category CAT."
    (cl-case cat
      (file (all-the-icons-completion-get-file-icon cand))
      (project-file (all-the-icons-completion-get-file-icon cand))
      (buffer (all-the-icons-completion-get-buffer-icon cand))
      (project-buffer (all-the-icons-completion-get-buffer-icon cand))
      (t ""))))

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Support for editorconfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode t))

(use-package diff-hl
  :straight t
  :hook
  (prog-mode . diff-hl-mode))

;; GIT client
(use-package magit
  :straight t
  :bind ("C-c g" . magit-status))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

;; Snippets
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :straight t
  :config
  (yasnippet-snippets-initialize))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :straight t
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          e          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode))

(use-package consult-project-extra
  :straight t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package highlight-indent-guides
  :defer t
  :commands highlight-indent-guides-mode
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-even-face-perc 3)
  (highlight-indent-guides-auto-odd-face-perc 2.5)
  (highlight-indent-guides-auto-top-even-face-perc 12)
  (highlight-indent-guides-auto-top-odd-face-perc 10)
  (highlight-indent-guides-character ?\u2502)
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top))

(use-package undo-tree
  :straight t
  :diminish undo-tree
  :custom
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

(defun undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply original-function args)))
(advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

(defun nolinum ()
  (global-display-line-numbers-mode -1))
(add-hook 'undo-tree-visualize 'nolinum)

(use-package minions
  :straight t
  :config
  (minions-mode t))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))


(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))


(use-package aggressive-indent
  :straight t
  :hook
  (prog-mode . aggressive-indent-mode))

(provide 'editor)
;;; editor.el ends here
