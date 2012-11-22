(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"

  ;; Evil key binding
  (and (evil-define-key 'motion coffee-mode-map (kbd "jc") 'coffee-compile-buffer)
       (evil-define-key 'visual coffee-mode-map (kbd "jc") 'coffee-compile-region)))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

(add-hook 'after-save-hook
          '(lambda () (if (string= major-mode "coffee-mode") (coffee-compile-file) ())))

(provide 'danr-coffee)
