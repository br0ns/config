;; My functions

(defun duplicate-line ()
  (interactive)
  (let (
        (c (current-column))
        )
    (progn
      (kill-ring-save
       (line-beginning-position)
       (line-end-position)
       )
      (end-of-line)
      (insert-char ?\n 1)
      (yank)
      (beginning-of-line)
      (forward-char c)
      )
    )
  )

(defun flip-next ()
  (interactive)
  (let* (
        (b1 (window-buffer))
        (w2 (next-window))
        (b2 (window-buffer w2))
        )
    (switch-to-buffer b2 nil)
    (select-window w2)
    (switch-to-buffer b1 nil)
    (other-window -1)
    )
  )

(defun flip-previous ()
  (interactive)
  (let* (
        (b1 (window-buffer))
        (w2 (previous-window))
        (b2 (window-buffer w2))
        )
    (switch-to-buffer b2 nil)
    (select-window w2)
    (switch-to-buffer b1 nil)
    (other-window 1)
    )
  )


(defun insert-zero ()
  "Insert a zero byte -- hack for = and quotes in org-mode"
  (interactive)
  (insert 22)
  )

(defun smart-split ()
  "Horizontally split into as many frames smart-split-columns wide as possible."
  (interactive)
  (let* (
        ;; (edge (- (nth 2 (window-edges)) (window-width)))
        (edge 4)
        (numw (/ (window-width) (+ fill-column edge)))
        )
    (defun loop (n w)
      (if (> n 1)
          (let* (
                 (width (+ (window-width w) edge))
                 (w2 (split-window w (/ width n) t))
                 )
            (loop (- n 1) w2)
            )
        )
      )
    (loop numw nil)
    )
  )

(defun new-note (note)
  "Makes a new note (org-mode) in ~/notes"
  (interactive "sNew note: ")
  (let (
        (filename (concat "~/notes/"
                          (format-time-string "%Y-%m-%d(%H:%m)" (current-time))
                          ":" note ".org"
                          )
                  )
        )
    (find-file filename)
    (insert (concat "#+TITLE " note))
    )
  )

(defun yank-as-rectangle ()
  (interactive)
  (insert-rectangle
   (split-string (current-kill 0) "\n")
   )
  )

(defun end-of-line-and-newline ()
  (interactive)
  (end-of-line)
  (newline)
  )

(defun beginning-of-line-and-newline ()
  (interactive)
  (let
      (
       (pc (current-column))
       )
    (progn
      (beginning-of-line)
      (newline)
      (forward-char pc)
      )
    )
  )

(defun indent-region-or-line ()
  "Ident region if mark is active"
  (interactive)
  (if mark-active
      (let ((deactivate-mark nil))
        (indent-region (region-beginning)
                       (region-end))
        )
    (indent-according-to-mode)
    )
  )

(defun increase-indentation ()
  "Increase identation of region if mark is active"
  (interactive)
  (if mark-active
      (let ((deactivate-mark nil))
        (increase-left-margin)
        )
    (increase-left-margin (line-beginning-position) (line-end-position))
    )
  )

(defun decrease-indentation ()
  "Increase identation of region if mark is active"
  (interactive)
  (if mark-active
      (let ((deactivate-mark nil))
        (increase-left-margin)
        )
    (increase-left-margin (line-beginning-position) (line-end-position))
    )
  )

(defun hyper-tab ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (indent-region-or-line)
    )
  )

(defun go-to-next-paren ()
  "Go to next matching parentheses"
  (interactive)
  (forward-list)
  (backward-char)
  )

(defun go-to-previous-paren ()
  "Go to previous matching parentheses"
  (interactive)
  (backward-list)
  (forward-char)
  )

(defun prettify ()
  "Untabify and delete trailing whitespace"
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  )

(defun indent-whole-buffer ()
  "Ident the whole buffer, and untabify"
  (interactive)
  (prettify)
  (indent-region (point-min) (point-max) nil)
  )

(defun comment-or-uncomment-region-or-line ()
  "Copys region before (un)commenting if mark is active"
  (interactive)
  (if mark-active
      (progn
        (kill-ring-save (region-beginning)
                        (region-end)
                        )
        (comment-or-uncomment-region (region-beginning)
                                     (region-end)
                                     )
        )
      (progn
        (kill-ring-save (line-beginning-position)
                        (+ (line-end-position) 1)
                        )
        (comment-or-uncomment-region (line-beginning-position)
                                     (line-end-position)
                                     )
        )
    )
  )

(defun scroll-up-one ()
  "Scroll one line"
  (interactive)
  (scroll-up 1)
  )

(defun scroll-down-one ()
  "Scroll one line"
  (interactive)
  (scroll-down 1)
  )

(defun set-title ()
  "Sets the title of the frame to the current file or the buffer name if not visiting a file"
  ;; (if (buffer-file-name)
      ;; (setq frame-title-format "%f")
    (setq frame-title-format (buffer-name))
    ;; )
  )

(defun go-to-next-window ()
 "Go to next window"
 (interactive)
 (other-window 1)
 (set-title)
 )

(defun go-to-prev-window ()
  "Go to previous window"
  (interactive)
  (other-window -1)
  (set-title)
  )

(defun transpose-up ()
  "Moves line or selected text up"
  (interactive)
  (if mark-active
      (t)
    (
     (transpose-lines 1)
     (previous-line)
     (previous-line)
     )
    )
  )

(defun pager-half-page-up ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (pager-scroll-screen (/ (window-height) 2))
  )

(defun pager-half-page-down ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (pager-scroll-screen (- (/ (window-height) 2)))
  )

(defun scroll-next-line ()
  (interactive)
  ;; (next-line)
  ;; (recenter)
  (pager-scroll-screen 1)
)

(defun scroll-previous-line ()
  (interactive)
  ;; (previous-line)
  ;; (recenter)
  (pager-scroll-screen -1)
)

(defun kill-region-or-line ()
  "Kills region or line, if mark is not active"
  (interactive)
  (if mark-active
      (kill-region (mark) (point))
    (kill-region
     (line-beginning-position)
     (line-beginning-position 2)
     )
    )
  )

(defun kill-ring-save-region-or-line ()
  "Saves region or line, if mark is not active"
  (interactive)
  (if mark-active
      (kill-ring-save (mark) (point))
    (kill-ring-save
     (line-beginning-position)
     (line-beginning-position 2)
     )
    )
  )

(defun move-line-or-lines (&optional n)
  (interactive)
  (when (null n)
    (setq n 1))
  (let (
        (pc (current-column))
        (deactivate-mark nil)
        )
    (if mark-active
        (progn
          (exchange-point-and-mark)
          (let (
                (mc (current-column))
                (pl (line-number-at-pos (mark)))
                (ml (line-number-at-pos (point)))
                )
            (if (< pl ml)
                (progn
                  (goto-line pl)
                  (beginning-of-line)
                  (set-mark-command nil)
                  (goto-line ml)
                  (end-of-line)
                  )
              (progn
                (goto-line ml)
                (beginning-of-line)
                (set-mark-command nil)
                (goto-line pl)
                (end-of-line)
                )
              )
            (forward-char 1)
            (kill-region (mark) (point))
            (next-line n)
            (beginning-of-line)
            (yank)
            (goto-line (+ ml n))
            (forward-char mc)
            (set-mark (point))
            (goto-line (+ pl n))
            (forward-char pc)
            )
          )
      (progn
        (beginning-of-line)
        (next-line 1)
        (transpose-lines n)
        (previous-line 1)
        (forward-char pc)
        )
      )
    )
  )

(defun move-line-or-lines-one-up ()
  (interactive)
  (move-line-or-lines -1)
  )

(defun move-line-or-lines-one-down ()
  (interactive)
  (move-line-or-lines 1)
  )


(defun reload () (interactive)
  "Reload .emacs"
  (if (file-exists-p "~/.emacs")
      (load-file "~/.emacs")
    )
  )

;; (defun rename-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;       (if (get-buffer new-name)
;;           (message "A buffer named '%s' already exists!" new-name)
;;         (progn (rename-file name new-name 1)
;;                (rename-buffer new-name)
;;                (set-visited-file-name new-name)
;;                (set-buffer-modified-p nil)
;;                )
;;         )
;;       )
;;     )
;;   )

;; (defun move-file-and-buffer ()
;;   (interactive)
;;   (if (not (buffer-file-name))
;;       (call-interactively 'rename-buffer)
;;     (let ((file (buffer-file-name)))
;;       (with-temp-buffer
;;         (set-buffer (dired-noselect file))
;;         (dired-do-rename)
;;         (kill-buffer nil))))
;;   nil)

(defun move-file-and-buffer (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1)
            dir
            )
          )
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t)
      )
    )
  )

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename."))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s" new-name)))

;; Date and time functions copied from
;; http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )