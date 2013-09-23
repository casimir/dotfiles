(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
;; (load-file "~/.cabal/share/Agda-2.3.1/emacs-mode/agda2.el")

;; Agda evil bindings
(evil-define-key 'normal agda2-mode-map
   "gl" 'agda2-load
   "g." 'agda2-goal-and-context-and-inferred
   "g," 'agda2-goal-and-context
   "gr" 'agda2-refine
   "g=" 'agda2-show-constraints
   "g?" 'agda2-show-goals
   "gc" 'agda2-make-case
   "ga" 'agda2-auto
   "gf" 'agda2-next-goal
   "gp" 'agda2-previous-goal
   "g " 'agda2-give
   "\\s" 'agda2-solveAll)

(setq agda2-include-dirs '("." "~/build/lib/src"))

(provide 'danr-agda)
