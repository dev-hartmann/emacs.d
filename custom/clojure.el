;;; cljojure.el
(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

(use-package cider
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli))

(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))


(provide 'clojure)
;;; clojure.el ends here
