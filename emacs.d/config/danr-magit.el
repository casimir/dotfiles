(require 'magit)

;; TODO: remove this copy-paste

;; magit starts in emacs
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'magit-key-mode 'emacs)
(evil-set-initial-state 'magit-log-mode 'emacs)

(eval-after-load 'magit
  '(and (define-key magit-mode-map (kbd "t") 'magit-goto-next-section)
        (define-key magit-mode-map (kbd "n") 'magit-goto-previous-section)
        (define-key magit-mode-map (kbd "T") 'magit-key-mode-popup-tagging)
        (define-key magit-mode-map (kbd "h") 'magit-toggle-diff-refine-hunk)
        (define-key magit-mode-map (kbd "s") 'magit-stage-item)
        (define-key magit-mode-map (kbd "V") 'evil-visual-line)
        (define-key magit-mode-map (kbd "j") (lookup-key evil-motion-state-map "j"))
        (define-key magit-mode-map (kbd "g") (lookup-key evil-motion-state-map "g"))
        (define-key magit-mode-map (kbd "<tab>") (lookup-key evil-motion-state-map [tab]))
        (define-key magit-mode-map (kbd ";") (lookup-key magit-mode-map ":"))))

(evil-define-key 'visual magit-mode-map
  [escape] 'evil-emacs-state
  "s"      'magit-stage-item)

;; in commit edit mode, override g# and define jc
(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-define-key 'normal magit-log-edit-mode-map
  "g#" 'magit-log-edit-commit
  "jc" 'magit-log-edit-cancel-log-message)

(provide 'danr-magit)
