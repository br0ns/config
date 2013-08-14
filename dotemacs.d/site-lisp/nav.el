;; Remember temporary-goal-column even when scrolling and jumping by blank-lines
;; TODO: get to work with track-eol.el

(defvar nav-temporary-goal-column 0)

(defun forward-blank-line (&optional arg)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive)
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR")
      (progn (backward-char))
    (progn (goto-char (point-max)) )
    )
  )

(defun backward-blank-line (&optional arg)
  "Move cursor backward to previous text block.
See: `ergoemacs-forward-block'"
  (interactive)
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
      (progn
        (skip-chars-backward "\n\t ")
        (forward-char 1)
        )
    (progn (goto-char (point-min)) )
    )
  )

(defun nav-update-goal-column ()
  "Update `nav-temporary-goal-column' if necessary."
  (unless (memq last-command
                '(nav-scroll-forward-blank-line
                  nav-scroll-backward-blank-line
                  nav-scroll-forward-line
                  nav-scroll-backward-line
                  nav-forward-blank-line
                  nav-backward-blank-line
                  nav-forward-line
                  nav-backward-line
                  nav-page-down
                  nav-page-up
                  nav-half-page-down
                  nav-half-page-up
                  ))
    (setq nav-temporary-goal-column (current-column))
    )
  )

(defun nav-move-to-column (column)
  "Like `move-to-column' but cater for wrapped lines."
  (progn
    (if (or (bolp)
            ;; Start of a screen line.
            (not (zerop (mod (- (point) (line-beginning-position))
                             (window-width)))))
        (move-to-column column)
      (forward-char (min column (- (line-end-position) (point)))))
    )
  )

(defun nav-move-to-goal-column ()
  (nav-move-to-column nav-temporary-goal-column)
  )

(defun nav-can-scroll-up ()
  (> (line-number-at-pos (window-start)) 1)
  )

(defun nav-can-scroll-down ()
  (< (window-end) (buffer-end 1))
  )

;; Borrowed from nav.el
(defun nav-scroll-screen (lines)
  "Scroll screen LINES, but keep the cursors position on screen."
  (nav-update-goal-column)
  (save-excursion
    (goto-char (window-start))
    (forward-line lines)
    (set-window-start (selected-window) (point)))
  (forward-line lines)
  (nav-move-to-goal-column)
  )

(defun nav-scroll-forward-blank-line (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (if (nav-can-scroll-down)
      (let ((saved-point (point)))
        (progn
          (nav-update-goal-column)
          (forward-blank-line arg)
          (scroll-up (count-screen-lines saved-point (point)))
          (nav-move-to-goal-column)
          )
        )
    )
  )

(defun nav-scroll-backward-blank-line (&optional arg)
  "Scroll down ARG blank-lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (if (nav-can-scroll-up)
      (let ((saved-point (point)))
        (progn
          (nav-update-goal-column)
          (backward-blank-line arg)
          (condition-case nil
              (scroll-down (count-screen-lines (point) saved-point))
            (beginning-of-buffer (goto-char (point)))
            )
          (nav-move-to-goal-column)
          )
        )
    )
  )

(defun nav-scroll-forward-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (nav-can-scroll-down)
      (progn
        (nav-update-goal-column)
        (scroll-up arg)
        (forward-line arg)
        (nav-move-to-goal-column)
        )
    )
  )

(defun nav-scroll-backward-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (nav-can-scroll-up)
      (progn
        (nav-update-goal-column)
        (scroll-down arg)
        (forward-line (- arg))
        (nav-move-to-goal-column)
        )
    )
  )

(defun nav-forward-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (nav-update-goal-column)
  (forward-line arg)
  (nav-move-to-goal-column)
  )

(defun nav-backward-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (nav-update-goal-column)
  (forward-line (- arg))
  (nav-move-to-goal-column)
  )

(defun nav-backward-blank-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (nav-update-goal-column)
  (backward-blank-line arg)
  (nav-move-to-goal-column)
  )

(defun nav-forward-blank-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (nav-update-goal-column)
  (forward-blank-line arg)
  (nav-move-to-goal-column)
  )

(defun nav-half-page-up ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (nav-update-goal-column)
  (nav-scroll-screen (- (/ (window-height) 2)))
  (nav-move-to-goal-column)
  )

(defun nav-half-page-down ()
  "Moves buffer half a page, without moving point"
  (interactive)
  (nav-update-goal-column)
  (nav-scroll-screen (/ (window-height) 2))
  (nav-move-to-goal-column)
  )

(defun nav-page-up ()
  "Moves buffer a page, without moving point"
  (interactive)
  (nav-update-goal-column)
  (nav-scroll-screen (- (window-height)))
  (nav-move-to-goal-column)
  )

(defun nav-page-down ()
  "Moves buffer a page, without moving point"
  (interactive)
  (nav-update-goal-column)
  (nav-scroll-screen (window-height))
  (nav-move-to-goal-column)
  )

(provide 'nav)