;; common settings

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
  make-backup-files nil

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

  ;; long linum format
  linum-format "%3d "

  ;; ignore extensions in ido mode
  completion-ignored-extensions '(".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")

)

;; truncate lines: don't wrap lines when they are too long
(toggle-truncate-lines t)
(setq-default truncate-lines t)

;; 0px fringes
(set-fringe-mode '(0 . 0))

;; save position in files
(setq-default save-place t)

;; no scroll bar
(scroll-bar-mode -1)

;; no toolbar
(tool-bar-mode -1)

;; no menu bar
(menu-bar-mode -1)

;; display current column
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

;; dired dosn't make new buffer when entering a folder
(put 'dired-find-alternate-file 'disabled nil)

;; make cursor not blink
(blink-cursor-mode 0)

;; set font
(set-default-font "dejavu sans mono-9")

;; also in new frames
(add-to-list 'default-frame-alist '(font . "dejavu sans mono-9"))

;; remove all trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't use goal column
(put 'set-goal-column 'disabled nil)

;; mixedCase to small_words_with_underscores (visually) with glasses-mode
(setq glasses-separate-parentheses-p nil
      glasses-uncapitalize-p t
      glasses-uncapitalize-regexp "[a-zA-Z]")

(provide 'danr-common)
