
(setq debug-on-error t)

(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

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

;; (require 'danr-haskell)
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


;; (load-file "~/build/Isabelle2013-2/contrib/ProofGeneral-4.2/generic/proof-site.el")
;; (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
(load-file "~/build/Isabelle2013-2/contrib/ProofGeneral-4.2/generic/proof-site.el")

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                (shell-command-to-string "agda-mode locate")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("." "~/build/lib/src" "/home/dan/build/lib/src/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
