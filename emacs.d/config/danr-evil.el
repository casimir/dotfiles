;; Evil mode

(require 'undo-tree)
(require 'evil)

(global-undo-tree-mode)

;; exit undo tree visualiser (enter with "gu") by pressing escape
(define-key undo-tree-visualizer-map [escape] 'undo-tree-visualizer-quit)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit"
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map           [escape] 'keyboard-quit)
(define-key evil-visual-state-map           [escape] 'keyboard-quit)
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

;; Scroll up
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; Tags
(define-key evil-motion-state-map (kbd "M-.") 'find-tag)
(define-key evil-motion-state-map (kbd "C-.") 'find-tag-other-window)

;; Abort search on escape
(define-key isearch-mode-map [escape] 'isearch-exit)
(define-key evil-ex-search-keymap [escape] 'abort-recursive-edit)

;; Dvorak movement

(define-key evil-normal-state-map "s" nil)

(define-key evil-motion-state-map "t" 'evil-next-line)
(define-key evil-motion-state-map "n" 'evil-previous-line)
(define-key evil-motion-state-map "s" 'evil-forward-char)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "N" 'evil-search-next)
(define-key evil-motion-state-map "T" 'evil-search-previous)
(define-key evil-motion-state-map "k" 'evil-find-char-to)
(define-key evil-motion-state-map "K" 'evil-find-char-to-backward)

(define-key evil-normal-state-map "l" 'evil-substitute)
(define-key evil-normal-state-map "-" 'newline-and-indent)

(define-key evil-motion-state-map "j" nil)

;; Some convenience on g/j: changing buffer, opening new files
(define-key evil-motion-state-map "jb" 'ido-switch-buffer)   ;; jump buffer
(define-key evil-motion-state-map "jr" 'revert-buffer)
(define-key evil-motion-state-map "jw" 'save-buffer)
(define-key evil-motion-state-map "gt" 'next-buffer)
(define-key evil-motion-state-map "gT" 'previous-buffer)
(define-key evil-motion-state-map "go" 'ido-find-file)       ;; go open
(define-key evil-motion-state-map "gk" 'kill-buffer)         ;; go kill
(define-key evil-motion-state-map "gu" 'undo-tree-visualize) ;; go undo-tree

;; windows management on g
(define-key evil-motion-state-map "gs" 'evil-window-right)
(define-key evil-motion-state-map "gh" 'evil-window-left)
(define-key evil-motion-state-map "gn" 'evil-window-next)
(define-key evil-motion-state-map "g2" 'split-window-vertically)
(define-key evil-motion-state-map "g3" 'split-window-horizontally)
(define-key evil-motion-state-map "g1" 'delete-other-windows)
(define-key evil-motion-state-map "g0" 'delete-window)

;; evaluating lisp on g{e/E}
(define-key evil-motion-state-map "ge" 'eval-last-sexp)
(define-key evil-motion-state-map "gE" 'eval-buffer)

;; finishing git commit messages on g#
(define-key evil-motion-state-map "g#" 'server-edit)

;; misc
(define-key evil-motion-state-map "g*" 'find-file-at-point)
(define-key evil-normal-state-map "g:" 'goto-last-change-reverse)
(define-key evil-motion-state-map "g8" 'what-cursor-position)

;; Search and replace
(define-key evil-normal-state-map "g5" 'query-replace)
(define-key evil-normal-state-map "g%" 'query-replace-regexp)

;; Dvorak window management (unused)

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

(provide 'danr-evil)
