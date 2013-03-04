;; Clean up GUI
(tooltip-mode -1)
(blink-cursor-mode -1)
(set-fringe-mode 0)

;; Set colors
(require 'color-theme-tango)
(color-theme-tango)

;; Set font
(set-frame-font "Liberation Mono-7")

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Allow upcase-region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Allow set-goal-column
(put 'set-goal-column 'disabled nil)

;; Display time in mode line
(display-time)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Tab width
(setq-default tab-width 2)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Mouse yank at point
(setq mouse-yank-at-point t)

;; Default to text mode
(setq-default major-mode 'text-mode)

;; Save sessions elsewhere
(desktop-change-dir emacs-file-dir)

;; Enable backup files
(setq make-backup-files t)

;; Save all backup file in this directory
(setq backup-directory-alist `((".*" .
                                ,(expand-file-name "backup" emacs-file-dir)))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Keep autosave list here
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" emacs-file-dir))

;; Set title when window configuration changes
(defun set-title ()
  (setq frame-title-format (buffer-name))
  )
(add-hook 'window-configuration-change-hook 'set-title)
(add-hook 'after-init-hook 'set-title)

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

;; Highlight tabs and trailing whitespace
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; Show fill column
(set-default 'fill-column 80)
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-color "#2e3436") ; move this to color theme some time
(fci-mode)

;; Fill rules
(require 'filladapt)
(setq-default filladapt-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Default major mode
(setq major-mode 'text-mode)

;; Show matching parenthisis
(require 'mic-paren)
(paren-activate)

;; Increment/decrement numbers
(require 'evil-numbers)

;; Use Smex for M-x
(require 'smex)
(smex-initialize)

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Multiple cursers
(require 'multiple-cursors)
(add-hook 'before-save-hook 'mc/keyboard-quit)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "~/.emacs.files/places"))

;; Diminish modeline clutter
(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'filladapt-mode)
(diminish 'auto-fill-function)

;; Expand region
(require 'expand-region)

;; Save a list of recent files visited
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Choose one of these:
;; Don't break lines for me, please
(setq-default truncate-lines t)
;; Navigate by visual lines
;; (visual-line-mode)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Represent undo-history as an actual tree
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Bookmars
(setq bm-restore-repository-on-load t)
(require 'bm)
;; Make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
;; (add-hook 'after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Multifiles
(require 'multifiles)

;; Browse kill ring
(require 'browse-kill-ring)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Load viper for the viper-(forward|backward)-word functions
(setq viper-mode nil)
(require 'viper)

;; "Workspaces" for Emacs
(require 'perspective)
;; Enable perspective mode
(persp-mode t)

;; TODO: implement persp-last as before-advice on persp-switch (?)
(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))

;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

;; Quickly jump in buffers with ace-jump
(require 'ace-jump-mode)

;; Move current line or region
(require 'move-text)

;; Semantic navigation
(require 'smart-forward)

;; Remember my temporary-goal-column dammit
(require 'nav)
(defadvice nav-backward-line
  (after smooth-scroll-down
         (&optional arg try-vscroll)
         activate)
  (smooth-scroll-down)
  )

(defadvice nav-forward-line
  (after smooth-scroll-up
         (&optional arg try-vscroll)
         activate)
  (smooth-scroll-up)
  )

(defadvice nav-backward-paragraph
  (after smooth-scroll-down
         (&optional arg try-vscroll)
         activate)
  (smooth-scroll-down)
  )

(defadvice nav-forward-paragraph
  (after smooth-scroll-up
         (&optional arg try-vscroll)
         activate)
  (smooth-scroll-up)
  )


(provide 'setup-misc)
