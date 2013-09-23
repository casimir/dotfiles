(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; flyspell comments in Haskell
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; start my indentation
(load "~/.elisp/my-haskell-indentation.el")

(defun my-haskell-fill-comment ()
  (interactive)
  (just-one-space)
  (insert (make-string (- 79 (current-column)) ?-)))

(global-set-key (kbd "C-c C--") 'my-haskell-fill-comment)

(setq auto-mode-alist
   (cons '("\.hs" . haskell-mode) auto-mode-alist))

(setq auto-mode-alist
   (cons '("\.lhs" . literate-haskell-mode) auto-mode-alist))

(provide 'danr-haskell)
