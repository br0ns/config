;; == Selected default key binginds ==
;; M-c              `capitalize-word'
;; M-l              `downcase-word'
;; M-u              `upcase-word'
;; C-x r t          `string-rectangle'
;; C-x r k          `kill-rectangle'
;; C-x r y          `yank-rectangle'
;; f3               `kmacro-start-macro-or-insert-counter'
;; f4               `kmacro-end-or-call-macro'
;; M-0 f4           Repeat macro until bell

;; == General key bindings ==

;; == ido.el ==

;; == undo-tree.el ==
;; C-_  C-/         `undo-tree-undo'
;; M-_  C-?         `undo-tree-redo'
;; C-x u            `undo-tree-visualize'
;; C-x r u          `undo-tree-save-state-to-register'
;; C-x r U          `undo-tree-restore-state-from-register'
;; In the undo-tree visualizer:
;; <up>  p  C-p     `undo-tree-visualize-undo'
;; <down>  n  C-n   `undo-tree-visualize-redo'
;; <left>  b  C-b   `undo-tree-visualize-switch-branch-left'
;; <right>  f  C-f  `undo-tree-visualize-switch-branch-right'
;; C-<up>  M-{      `undo-tree-visualize-undo-to-x'
;; C-<down>  M-}    `undo-tree-visualize-redo-to-x'
;; <down>  n  C-n   `undo-tree-visualize-redo'
;; t                `undo-tree-visualizer-toggle-timestamps'
;; d                `undo-tree-visualizer-toggle-diff'
;; s                `undo-tree-visualizer-selection-mode'
;; q                `undo-tree-visualizer-quit'
;; C-q              `undo-tree-visualizer-abort'

;; == browse-kill-ring.el ==
;; M-y              `yank-pop'
;; q                `browse-kill-ring-quit'

;; Navigation
(global-set-key (kbd "<up>")             'nav-backward-line)
(global-set-key (kbd "<down>")           'nav-forward-line)
(global-set-key (kbd "C-<up>")           'nav-backward-blank-line)
(global-set-key (kbd "C-<down>")         'nav-forward-blank-line)
(global-set-key (kbd "M-<up>")           'nav-scroll-backward-line)
(global-set-key (kbd "M-<down>")         'nav-scroll-forward-line)
(global-set-key (kbd "C-M-<down>")       'nav-scroll-forward-blank-line)
(global-set-key (kbd "C-M-<up>")         'nav-scroll-backward-blank-line)
(global-set-key (kbd "M-S-<up>")         'scroll-down-one)
(global-set-key (kbd "M-S-<down>")       'scroll-up-one)
(global-set-key (kbd "<prior>")          'nav-page-up)
(global-set-key (kbd "<next>")           'nav-page-down)
(global-set-key (kbd "<XF86Back>")       'nav-half-page-up)
(global-set-key (kbd "<XF86Forward>")    'nav-half-page-down)
(global-set-key (kbd "M-g")              'goto-line-with-feedback)
(global-set-key (kbd "C-{")              'backward-sexp)
(global-set-key (kbd "C-}")              'forward-sexp)
(global-set-key (kbd "M-{")              'smart-backward)
(global-set-key (kbd "M-}")              'smart-forward)
(global-set-key (kbd "C-<left>")         '(lambda()
                                            (interactive)
                                            (forward-same-syntax -1)))
(global-set-key (kbd "C-<right>")        'forward-same-syntax)

;; "Free" combinations
;; M-[
;; M-]

;; For each complete line between point and mark, move to the beginning
;; of the line, and run the last keyboard macro.
(global-set-key (kbd "C-<f4>")           'apply-macro-to-region-lines)

;; Join next line with current
(global-set-key (kbd "M-j")              '(lambda ()
                                            (interactive)
                                            (join-line -1)))

;; Toggle between beginning of line and indent level
(global-set-key (kbd "M-m")              'back-to-indentation-or-beginning)

;; Toggle quotes
(global-set-key (kbd "C-\"")             'toggle-quotes)

;; Move lines or region
(global-set-key (kbd "C-S-<up>")         'move-text-up)
(global-set-key (kbd "C-S-<down>")       'move-text-down)

;; Clever newlines
(global-set-key (kbd "<S-return>")       'open-line-below)
(global-set-key (kbd "<M-return>")       'open-line-above)
(global-set-key (kbd "<M-S-return>")     'new-line-in-between)

;; Replace sexp by result of evaluation
(global-set-key (kbd "C-c C-e")          'fc-eval-and-replace)

;; Buffer file functions
(global-set-key (kbd "C-x t")            'touch-buffer-file)
(global-set-key (kbd "C-x C-r")          'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k")          'delete-current-buffer-file)

;; Window switching
(windmove-default-keybindings)
(global-set-key (kbd "C-S-o")            'rotate-windows)
(global-set-key (kbd "C-x C-|")          'toggle-window-split)
(global-set-key (kbd "C-o")              'go-to-next-window)
(global-set-key (kbd "C-M-o")            'go-to-prev-window)

;; Rectangles
(global-set-key (kbd "C-S-y")            'yank-rectangle)
(global-set-key (kbd "C-S-w")            'kill-rectangle)
(global-set-key (kbd "M-W")              'copy-rectangle)
(global-set-key (kbd "C-S-i")            'string-rectangle)

;; File finding
(global-set-key (kbd "C-x M-f")          'ido-find-file-other-window)
(global-set-key (kbd "C-x f")            'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p")          'find-or-create-file-at-point)
(global-set-key (kbd "C-c y")            'bury-buffer)
(global-set-key (kbd "C-c r")            'revert-buffer)

;; Edit file with sudo
(global-set-key (kbd "M-s e")            'sudo-edit)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>")       '(lambda ()
                                            (interactive)
                                            (revert-buffer t t)))

;; Misc
(global-set-key (kbd "M-SPC")            'hippie-expand)
(global-set-key (kbd "C-q")              'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-z")              'ido-switch-buffer)
(global-set-key (kbd "M-d")              'duplicate-current-line-or-region)
(global-set-key (kbd "C-S-a")            'align-all-strings)

(global-set-key (kbd "<f5>") 'shell)

(global-set-key (kbd "C-M-<right>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-M-<left>")       'evil-numbers/dec-at-pt)
(global-set-key (kbd "<f7>")             'clipboard-kill-ring-save)
(global-set-key (kbd "<f8>")             'clipboard-yank)
;; TODO: Indent current line if mark not active
(global-set-key (kbd "C->")              'increase-indentation)
(global-set-key (kbd "C-<")              'decrease-indentation)
(global-set-key (kbd "C-S-d")            'kill-word)

;; bm.el
(global-set-key (kbd "<f2>")             'bm-toggle)
(global-set-key (kbd "C-S-<right>")      'bm-next)
(global-set-key (kbd "C-S-<left>")       'bm-previous)

;; multifiles.el
(global-set-key (kbd "C-!")              'mf/mirror-region-in-multifile)

;; multiple-cursors.el
(global-set-key (kbd "C-S-SPC")          'set-rectangular-region-anchor)
(global-set-key (kbd "C-S-c C-S-c")      'mc/edit-lines)
(global-set-key (kbd "C-S-c C-c")        'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-.")              'mc/mark-next-like-this)
(global-set-key (kbd "C-M-.")            'mc/skip-and-mark-next-like-this)
(global-set-key (kbd "C-,")              'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-,")            'mc/skip-and-mark-previous-like-this)
(global-set-key (kbd "C-;")              'mc/mark-all-in-region)
(global-set-key (kbd "C-:")              'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-c C-e")        'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a")        'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-M-'")            'mc/reverse-regions)
(global-set-key (kbd "C-M-\"")           'mc/sort-regions)

;; browse-kill-ring.el
(global-set-key (kbd "C-x C-y")          'browse-kill-ring)

;; zoom-frm.el
(global-set-key (kbd "s--")              'zoom-frm-out)
(global-set-key (kbd "s-=")              'zoom-frm-in)
(global-set-key (kbd "C-0")              'zoom-frm-unzoom)

;; smex.el
(global-set-key (kbd "M-x")              'smex)
(global-set-key (kbd "M-X")              'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x")      'execute-extended-command)

;; ido.el
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "C-z")   'ido-next-match)
  (define-key ido-completion-map (kbd "C-S-z") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<tab>") 'ido-complete)
  )
(add-hook 'ido-setup-hook 'ido-my-keys)

;; expand-region.el
(global-set-key (kbd "C-'")              'er/expand-region)
(global-set-key (kbd "C-M-'")            'er/contract-region)

;; perspective.el
(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)
(define-key persp-mode-map (kbd "C-x p") perspective-map)

;; ace-jump-mode.el
(define-key global-map (kbd "C-\\")      'ace-jump-mode)

(provide 'setup-bindings)