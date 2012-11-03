(defvar my-haskell-indent-tokens
  (quote ("do" "of" "="))
"The haskell tokens that make the next line's indentation increase."
;; should $ be in this list?
)

(defun my-current-indentation ()
"Returns the indentation the current line has."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward " ")
    (- (point) (line-beginning-position))))

(defun my-haskell-indent ()
"Indents the current line with my heuristic (look at line above)

 It's a bit unfortunate really, because if we're doing O, then we would want
 to look at the line underneath. Well, well, what can you do?

 This should set indent-line-function
"
  (interactive)
  (let ((new-indentation
         (save-excursion
           (beginning-of-line)
           (forward-line -1)
           (end-of-line)
           (let*
               ((last-indentation (my-current-indentation))
                (extra-indentation
                  (if (member (current-word) my-haskell-indent-tokens) 4 0)))
             (+ (* (ceiling last-indentation 4) 4) extra-indentation)))))
    (indent-to-column new-indentation)))



(defun my-decrease-tab-line ()
"Decreases the tab depth on this line"
  (interactive)
  (save-excursion
    (let* ((current-indentation (my-current-indentation))
           (target-indentation
            ;; (max 0 (* (floor (- current-indentation 1) 4) 4)))
            (max 0 (- current-indentation 4)))
           (chars-to-delete
            (- current-indentation target-indentation)))
      (goto-char (line-beginning-position))
      (delete-char chars-to-delete))))

(defun my-increase-tab-line ()
"Increase the tab depth on this line"
  (interactive)
  (save-excursion
    (let* ((current-indentation (my-current-indentation))
           (target-indentation (+ current-indentation 4)))
      (goto-char (line-beginning-position))
      (indent-to-column (- target-indentation current-indentation)))))

(defun my-foreach-line (fun)
"Executes a function on each line"
  (save-excursion
    (let ((deactivate-mark t))
      (let* ((start (region-beginning))
             (stop (- (region-end) 1)))
       (goto-char stop)
       (while (>= (point) start)
         (funcall fun)
         (forward-line -1))))))

(defun my-decrease-tab-selection ()
"Decreases the tab depth on current selection"
  (interactive)
  (my-foreach-line 'my-decrease-tab-line))

(defun my-increase-tab-selection ()
"Decreases the tab depth on current selection"
  (interactive)
  (my-foreach-line 'my-increase-tab-line))

(defun my-insert-tab-stop ()
"Indents the current position to the next tab stop.

 TODO: Make it insert until the next tab stop rather than just
       inserting four spaces."
  (interactive)
  (insert "    "))

(defun my-delete-tab-stop ()
"De-indents the current position to the last tab stop.

 TODO: Make it remove until the last tab stop rather than just
       removing four spaces."
  (interactive)
  (delete-backward-char 4))

;; Start my indentation tricks in haskell
(add-hook 'haskell-mode-hook
   '(lambda () (setq indent-line-function 'my-haskell-indent)))

;; Bind the tab/backtab keys
;; * Normal mode : reindents the current line
;; * Visual mode : reindents selection
;; * Insert mode : insert/remove 4 spaces
(evil-define-key 'normal haskell-mode-map
  (kbd "<tab>")     'my-increase-tab-line
  (kbd "<backtab>") 'my-decrease-tab-line)

(evil-define-key 'visual haskell-mode-map
  (kbd "<tab>")     'my-increase-tab-selection
  (kbd "<backtab>") 'my-decrease-tab-selection)

(evil-define-key 'insert haskell-mode-map
   (kbd "<tab>")     'my-insert-tab-stop
   (kbd "<backtab>") 'my-delete-tab-stop)




;; unused
(defun my-haskell-newline ()
"Makes a newline, and indents as much as the line above it is indented,
 but if the line above ends with a haskell indentation token, then
 indents it an extra tab.

 UNUSED: use setq 'indent-line-function 'my-haskell-indent instead
"
  (interactive)
  (let*
      ((last-indentation (my-current-indentation))
       (extra-indentation
         (if (member (current-word) my-haskell-indent-tokens) 4 0))
       (new-indentation
         (+ (* (ceiling last-indentation 4) 4) extra-indentation)))
    (list (newline)
          (indent-to-column new-indentation))))
