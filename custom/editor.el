;;; editor.el --- -*- lexical-binding: t; -*-
(setq-default indent-tabs-mode nil
              tab-width 8
              require-final-newline t
              sentence-end-double-space nil
              save-place-file "~/.emacs.d/backups/saveplace"
              fringes-outside-margins t
              custom-file "~/.emacs.d/.custom.el"
              vc-follow-symlinks t
              version-control t
              kill-ring-max 120)

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

(use-package github-browse-file)

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
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/programming")
    (setq projectile-project-search-path '("~/projects/programming")))
  :custom
  (projectile-buffers-filter-function 'projectile-buffers-with-file-or-process)
  (projectile-completion-system 'default)
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching nil)
  (projectile-globally-ignored-directories '(".bzr"
                                             ".eunit"
                                             ".extension"
                                             ".fslckout"
                                             ".git"
                                             ".hg"
                                             ".idea"
                                             ".svn"
                                             ".vagrant"
                                             "_darcs"
                                             "archive-contents"
                                             "cache"
                                             "coverage"
                                             "doc"
                                             "docs"
                                             "elpa"
                                             "log"
                                             "logs"
                                             "node_modules"
                                             "sorbet"
                                             "straight"
                                             "tmp"
                                             "vendor/assets"))
  (projectile-globally-ignored-files '("TAGS" "*.log"))
  (projectile-sort-order 'recently-active))

(defun dh/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult-projectile
  :straight t)

(use-package highlight-indent-guides
  :defer t
  :commands highlight-indent-guides-mode
  :diminish highlight-indent-guides-mode

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

(use-package whitespace-cleanup-mode
  :defer t
  :hook
  (prog-mode . whitespace-cleanup-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point nil)
  (whitespace-cleanup-mode-only-if-initially-clean nil))

(use-package minions
  :straight t
  :config
  (minions-mode t))

(use-package treemacs
  :straight t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          ;;treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    (treemacs-resize-icons 18)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :after (treemacs))

(provide 'editor)
;;; editor.el ends here
