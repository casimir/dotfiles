;; -- ELisp mode ---------------------------------------------------------------

;; eldoc in lisp
(add-hook 'lisp-mode 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "eL")))


(provide 'danr-elisp)
