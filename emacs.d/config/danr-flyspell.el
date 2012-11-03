(require 'flyspell)

;; Auto-fill and
(add-hook 'latex-mode-hook 'turn-on-flyspell 'append)

;; Start flyspell
(global-set-key (kbd "C-c s") 'flyspell-mode)

;; British dictionary
(setq ispell-dictionary "british-ise")

(provide 'danr-flyspell)
