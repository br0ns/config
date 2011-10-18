;; Ido mode
(require 'ido)
(ido-mode 1)

(defun ido-my-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map "\C-z" 'ido-next-match)
 (define-key ido-completion-map (kbd "C-S-z") 'ido-prev-match)
 (define-key ido-completion-map [tab] 'ido-complete)
 )

(add-hook 'ido-setup-hook 'ido-my-keys)
