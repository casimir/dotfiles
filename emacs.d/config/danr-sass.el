(add-to-loadpath "~/.elisp/sass-mode")

(require 'sass-mode)

(defun sass-custom ()
  "sass-mode-hook: change to preserve indent function"
  (setq indent-line-function 'my-haskell-indent))

(add-hook 'sass-mode-hook '(lambda () (sass-custom)))

(defun my-change-extension-to-css (file-name)
  "Changes file-name.ext to file-name.css"
  (concat (file-name-sans-extension file-name) ".css"))

(defun sass-compile-file ()
  "sass-save-hook: runs sass compile"
  (call-process "sass" nil "*sass*" nil
				(buffer-file-name)
				(my-change-extension-to-css (buffer-file-name))))

(add-hook 'after-save-hook
          '(lambda () (if (string= major-mode "sass-mode") (sass-compile-file))))

(provide 'danr-sass)
