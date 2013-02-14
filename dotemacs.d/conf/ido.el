;; Ido mode

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-use-filename-at-point 'guess)

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)

(setq ido-file-extensions-order '(".tex" ".org" ".txt" ".sml" ".hs" ".emacs"))
;; Enable ido to use the ignore-extensions variable
(setq ido-ignore-extensions t)
(setq completion-ignored-extensions '(".hi" ".so" ".o" ".aux" ".snm" ".nav" ".toc" ".vrb" ".pyc" ".elc"))