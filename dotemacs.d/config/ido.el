;; Ido mode
(require 'ido)
(ido-mode 1)

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s") 
  (lambda() 
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

(defun ido-my-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map "\C-z" 'ido-next-match)
 (define-key ido-completion-map (kbd "C-S-z") 'ido-prev-match)
 (define-key ido-completion-map [tab] 'ido-complete)
 )

(add-hook 'ido-setup-hook 'ido-my-keys)
