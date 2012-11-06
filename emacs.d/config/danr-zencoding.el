(require 'zencoding-mode)

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(define-key evil-normal-state-map "gx" 'zencoding-expand-line)

(provide 'danr-zencoding)
