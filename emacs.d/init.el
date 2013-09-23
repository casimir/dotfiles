
(defun add-to-loadpath (&rest dirs)
  "From cofi/dotfiles"
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.emacs.d/config"
                 "~/.elisp"
                 "~/.elisp/evil"
                 "~/.elisp/haskell-mode"
                 "~/.elisp/undo-tree")

(require 'danr-common)

(require 'danr-evil)

(require 'danr-haskell)
(require 'danr-agda)

;; (require 'danr-c)
;; (require 'danr-coffee)
;; (require 'danr-elisp)
;; (require 'danr-erlang)
;; (require 'danr-flyspell)
;; (require 'danr-magit)
;; (require 'danr-markdown)
;; (require 'danr-zencoding)
;;
;; (require 'danr-haml)
;; (require 'danr-sass)

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)
