;; Clean up GUI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Set colors
(require 'color-theme-tango)
(color-theme-tango)

;; Remove fringes
(set-fringe-mode 0)

;; Don't show splash screen
(setq inhibit-startup-message t)

;; Set font
(set-frame-font "Liberation Mono-7")

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Don't disable upcase-region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Display time in mode line
(display-time)

;; Use transient mark mode
(transient-mark-mode 1)

;; Tab width
(setq tab-width 2)

;; Column number in mode line
(setq column-number-mode t)

;; Don't use tabs dammit
(setq-default indent-tabs-mode nil)

;; Syntax colouring
(global-font-lock-mode t)

;; Mouse yank at point
(setq mouse-yank-at-point t)

;; Truncate lines if they are too long
(setq truncate-lines t)
;; Truncate even even when screen is split into multiple windows
(setq truncate-partial-width-windows nil)

;; Default to text mode
(setq major-mode 'text-mode)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Per window point
(require 'winpoint)
(winpoint-mode t)

;; Create dir if non-existing
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; Delete trailing white space when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook
          '(lambda ()
             (message (concat
                       "Wrote "
                       (buffer-file-name)
                       " (and removed trailing whitespace)"))))

;; Navigate by visual lines
(visual-line-mode)

;; Fix scrolling
(require 'smooth-scrolling)

;; Highlight tabs and trailing whitespace
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; Show fill column
(set-default 'fill-column 80)
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-color "#2e3436")
(fci-mode)

;; Fill rules
(require 'filladapt)
(setq-default filladapt-mode t)
(setq-default auto-fill-function 'do-auto-fill)
(setq comment-auto-fill-only-comments t)

;; Uniquely name buffers
(require 'uniquify)

;; Show matching parenthisis
(require 'mic-paren)
(paren-activate)

;; Increment/decrement numbers
(require 'evil-numbers)

;; Fix broken pgup/pgdw
(require 'pager)

;; Use Smex for M-x
(require 'smex)
(smex-initialize)

;; Hippie expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill))

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Multiple cursers
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/multiple-cursors"))
(require 'multiple-cursors)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "~/.emacs.backup/places"))

;; Diminish modeline clutter
(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'filladapt-mode)
(diminish 'auto-fill-function)

;; Expand region
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/expand-region"))
(require 'expand-region)
