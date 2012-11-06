
(defun add-to-loadpath (&rest dirs)
  "From cofi/dotfiles"
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.emacs.d/config"
                 "~/.elisp"
                 "~/.elisp/ac-dict"
                 "~/.elisp/ace-jump-mode"
                 "~/.elisp/auto-complete"
                 "~/.elisp/evil"
                 "~/.elisp/haskell-mode"
                 "~/.elisp/magit"
                 "~/.elisp/markdown-mode"
                 "~/.elisp/org-mode"
                 "~/.elisp/popup"
                 "~/.elisp/undo-tree")

(require 'danr-common)

(require 'danr-evil)

(require 'danr-agda)
(require 'danr-c)
(require 'danr-completion)
(require 'danr-elisp)
(require 'danr-erlang)
(require 'danr-flyspell)
(require 'danr-haskell)
(require 'danr-magit)
(require 'danr-markdown)

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)