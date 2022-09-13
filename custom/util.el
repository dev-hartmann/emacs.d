;;; util.el
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))


(global-set-key (kbd "s-<up>") 'move-line-up)
(global-set-key (kbd "s-<down>") 'move-line-down)

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))


(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

;; create buffer and move to it
(defun dh/v-split-and-move ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun dh/h-split-and-move ()
  (interactive)
  (split-window-below)
  (windmove-down))

(transient-define-prefix transient-lsp ()
  "LSP Code actions"
  [["Code Inspection"
    ("d" "find declaration"        lsp-find-declaration)
    ("g" "find definition"         lsp-ui-peek-find-definitions)
    ("i" "find implementation"     lsp-ui-peek-find-implementation)
    ("o" "describe thing at point" lsp-describe-thing-at-point)]
   ["Code actions"
    ("a" "code action"   lsp-execute-code-action)
    ("r" "rename point"  lsp-rename)
    ("f" "format buffer" lsp-format-buffer)]
   ["LSP server"
    ("s" "describe session"  lsp-describe-session)
    ("x" "restart workspace" lsp-restart-workspace)
    ("q" "shutdown server"   lsp-shutdown-workspace)]])

(transient-define-prefix transient-text-operations ()
  "Text ops"
  ["Zoom"
   ("g" "in" text-scale-increase :transient t)
   ("l" "out" text-scale-decrease :transient t)])
;; configuration languages etc.

;; Markdown
(use-package markdown-mode)
(use-package markdown-toc)
(use-package grip-mode
  :hook
  ((markdown-mode org-mode) . grip-mode))

(use-package yaml)

(use-package dashboard
  :config
  (setq dashboard-startup-banner 3
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-projects-backend 'project-el
        dashboard-items '((projects . 5)
                          (agenda . 5)
                          (recents  . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))
(provide 'util)
;;; util.el ends here  )
