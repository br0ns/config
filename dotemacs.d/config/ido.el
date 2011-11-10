;; Ido mode

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
;; (setq ido-enable-last-directory-history nil)
;; (setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
;; (setq ido-show-dot-for-dired t) ;; put . as the first item
;; (setq ido-use-filename-at-point t) ;; prefer file names near point

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
