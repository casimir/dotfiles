

(defun add-to-loadpath (&rest dirs)
  "From cofi/dotfiles"
  (dolist (dir dirs load-path)
    (add-to-list 'load-path (expand-file-name dir) nil #'string=)))

(add-to-loadpath "~/.elisp")
(add-to-loadpath "~/.elisp/ac-dict")
(add-to-loadpath "~/.elisp/ace-jump-mode")
(add-to-loadpath "~/.elisp/auto-complete")
(add-to-loadpath "~/.elisp/evil")
(add-to-loadpath "~/.elisp/haskell-mode")
(add-to-loadpath "~/.elisp/magit")
(add-to-loadpath "~/.elisp/markdown-mode")
(add-to-loadpath "~/.elisp/org-mode")
(add-to-loadpath "~/.elisp/popup")
(add-to-loadpath "~/.elisp/undo-tree")

;; -- Common settings ---------------------------------------------------------

;; run server
(server-start)

(setq
  ;; don't show the startup screen
  inhibit-startup-screen t

  ;; never follow symlinks and don't ask
  vc-follow-symlinks nil

  ;; see what you type
  echo-keystrokes 0.01

  ;; text scrolling
  scroll-conservatively 50
  scroll-preserve-screen-position 't
  scroll-margin 10

  ;; Insert space/slash after completion
  comint-completion-addsuffix t

  ;; number of chars in line
  fill-column 80

  ;; make sure file ends with NEWLINE
  require-final-newline t

  ;; delete excess backup versions
  delete-old-versions t

  ;; paste at cursor NOT at mouse pointer position
  mouse-yank-at-point t

  ;; display time in the modeline
  display-time-24hr-format t
  display-time-day-and-date nil

  ;; calendar customizing
  european-calendar-style t
  calendar-week-start-day 1

  ;; no "#" files after a save
  delete-auto-save-files t

  ;; NO annoing backups
  make-backup-files t

  ;; autosave every 512 keyboard inputs
  auto-save-interval 512

  ;; limit the number of newest versions
  kept-new-versions 5

  ;; limit the number of oldest versions
  kept-old-versions 5

  auto-save-list-file-prefix "~/.emacs.d/backups/save-"

  ;; cursors!!
  cursor-in-non-selected-windows t

  ;; work on PRIMARY rather than CLIPBOARD
  ;; (same clipboard that firefox/urxvt uses)
  x-select-enable-primary t

  ;; make ido ignore stupid extensions
  ido-ignore-extensions t

  ;; enable tramp (enables :ssh, :sudo etc)
  tramp-mode t

  ;; put space instead of a tab when using tab
  indent-tabs-mode nil

  ;; setting the default tabulation
  default-tab-width 4

  ;; make tabstops every 4th char
  tab-stop-list (number-sequence 0 80 4)

)

;; save position in files
(setq-default save-place t)

;;  no scroll bar
(scroll-bar-mode -1)

;; no toolbar
(tool-bar-mode -1)

;; no menu bar
(menu-bar-mode -1)

;; display current columen
(column-number-mode t)

;; display time and battery
(display-time)
(display-battery-mode t)

;; "y or n" instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; enable ido-mode
(ido-mode t)

;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; dired dosn't make new buffer when entering a map
(put 'dired-find-alternate-file 'disabled nil)

;; make cursor not blink
(blink-cursor-mode 0)

;; set font
(set-default-font "dejavu sans mono-9")

;; remove all trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't use goal column
(put 'set-goal-column 'disabled nil)




;; -- Undo-tree mode (used by evil) -------------------------------------------
(require 'undo-tree)

(global-undo-tree-mode)

;; exit undo tree visualiser (enter with "gu") by pressing escape
(define-key undo-tree-visualizer-map [escape] 'undo-tree-visualizer-quit)

;; -- Evil mode ----------------------------------------------------------------

(require 'evil)

;; Esc quits minibuffer

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit"
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-normal-state-map (kbd "M-.") 'find-tag)
(define-key evil-normal-state-map (kbd "C-.") 'find-tag-other-window)

(define-key evil-normal-state-map           [escape] 'keyboard-quit)
(define-key evil-visual-state-map           [escape] 'keyboard-quit)
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

;; Abort search on escape
(define-key isearch-mode-map [escape] 'isearch-exit)
(define-key evil-ex-search-keymap [escape] 'abort-recursive-edit)

;; Dvorak movement

(define-key evil-motion-state-map "t" 'evil-next-line)
(define-key evil-motion-state-map "n" 'evil-previous-line)
(define-key evil-motion-state-map "s" 'evil-forward-char)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-normal-state-map "l" 'evil-substitute)
(define-key evil-normal-state-map "jl" 'evil-change-whole-linee)
(define-key evil-normal-state-map "s" 'evil-forward-char)
(define-key evil-normal-state-map "-" 'newline-and-indent)
(define-key evil-motion-state-map "N" 'evil-search-next)
(define-key evil-motion-state-map "T" 'evil-search-previous)
(define-key evil-motion-state-map "k" 'evil-find-char-to)
(define-key evil-motion-state-map "K" 'evil-find-char-to-backward)

;; Some convenience on g/j: changing buffer, opening new files
(define-key evil-normal-state-map "jb" 'ido-switch-buffer)   ;; jump buffer
(define-key evil-normal-state-map "jr" 'revert-buffer)
(define-key evil-normal-state-map "jw" 'save-buffer)
(define-key evil-normal-state-map "gt" 'next-buffer)
(define-key evil-normal-state-map "gT" 'previous-buffer)
(define-key evil-normal-state-map "go" 'ido-find-file)       ;; go open
(define-key evil-normal-state-map "gk" 'kill-buffer)         ;; go kill
(define-key evil-normal-state-map "gu" 'undo-tree-visualize) ;; go undo-tree

;; Over-exploiting keybindings on g

;; windows management on g
(define-key evil-normal-state-map "gs" 'evil-window-right)
(define-key evil-normal-state-map "gh" 'evil-window-left)
(define-key evil-normal-state-map "gn" 'evil-window-next)
(define-key evil-normal-state-map "g2" 'split-window-vertically)
(define-key evil-normal-state-map "g3" 'split-window-horizontally)
(define-key evil-normal-state-map "g1" 'delete-other-windows)
(define-key evil-normal-state-map "g0" 'delete-window)

;; evaluating lisp on g{e/E}
(define-key evil-normal-state-map "ge" 'eval-last-sexp)
(define-key evil-normal-state-map "gE" 'eval-buffer)

;; finishing git commit messages on g#
(define-key evil-normal-state-map "g#" 'server-edit)

;; misc
(define-key evil-normal-state-map "g*" 'find-file-at-point)
(define-key evil-normal-state-map "g:" 'goto-last-change-reverse)
(define-key evil-normal-state-map "g8" 'what-cursor-position)

;; Search and replace
(define-key evil-normal-state-map "g5" 'query-replace)
(define-key evil-normal-state-map "g%" 'query-replace-regexp)

;; Dvorak window management

(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "H" 'evil-window-move-far-left)
(define-key evil-window-map "n" 'evil-window-down)
(define-key evil-window-map "N" 'evil-window-move-very-bottom)
(define-key evil-window-map "t" 'evil-window-up)
(define-key evil-window-map "T" 'evil-window-move-very-top)
(define-key evil-window-map "s" 'evil-window-right)
(define-key evil-window-map "S" 'evil-window-move-far-right)

(define-key evil-window-map "l" 'split-window-vertically)
(define-key evil-window-map "j" 'evil-window-top-left)

;; Align regexp in visual mode on l
(define-key evil-visual-state-map "l" 'align-regexp)

;; Make copy in visual mode not copy dangling char
(setq evil-want-visual-char-semi-exclusive t)


;; start evil mode
(evil-mode 1)


;; -- Erlang ------------------------------------------------------------------

;; (setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.7/emacs" load-path))
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

;; -- ELisp mode ---------------------------------------------------------------

;; eldoc in lisp
(add-hook 'lisp-mode 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "eL")))


;; -- Haskell mode -------------------------------------------------------------

;; haskell mode

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


;; config ---------------------------------------------------------------------

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))
          'append)





;; -- Auto-fill ---------------------------------------------------------------

;; Start auto-fill
(global-set-key (kbd "C-c f") 'auto-fill-mode)

(add-hook 'latex-mode-hook 'turn-on-auto-fill 'append)





;; -- Fly-spell ---------------------------------------------------------------

(require 'flyspell)

;; Auto-fill and
(add-hook 'latex-mode-hook 'turn-on-flyspell 'append)

;; Start flyspell
(global-set-key (kbd "C-c s") 'flyspell-mode)

;; British dictionary
(setq ispell-dictionary "british-ise")






;; -- Markdown mode --------------------------------------------------------------
(require 'markdown-mode)
(setq auto-mode-alist
   (cons '("\.md" . markdown-mode) auto-mode-alist))







;; -- Auto complete mode -------------------------------------------------------


(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Auto-complete in Haskell
(setq ac-modes (append ac-modes '(haskell-mode literate-haskell-mode)))






;; -- Agda mode ---------------------------------------------------------------
(load-file "~/.cabal/share/Agda-2.3.1/emacs-mode/agda2.el")

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






;; -- Custom variables --------------------------------------------------------
;; disable fringe: don't waste valuable space on edges
;; truncate lines: don't wrap lines when they are too long
;; ido mode ignore a lot of extensions
;; set how line-numbers should be viewed
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("." "~/build/lib/src")) t)
 '(completion-ignored-extensions (quote (".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(evil-auto-indent t)
 '(fringe-mode 0 nil (fringe))
 '(linum-format "%3d ")
 '(truncate-lines t))






;; ;; org mode in evil: remap org-mode meta keys for convenience
;; (mapcar (lambda (state)
;;        (evil-declare-key 'state org-mode-map
;;          (kbd "M-s") 'org-metaright
;;          (kbd "M-h") 'org-metaleft
;;          (kbd "M-n") 'org-metaup
;;          (kbd "M-t") 'org-metadown
;;          (kbd "M-S") 'org-shiftmetaright
;;          (kbd "M-H") 'org-shiftmetaleft
;;          (kbd "M-N") 'org-shiftmetaup
;;          (kbd "M-T") 'org-shiftmetadown))
;;      '(normal insert))
