;; Backup

;; Enable backup files.
(setq make-backup-files t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.backup/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

(setq auto-save-list-file-prefix "~/.emacs.backup/auto-save-list/.saves-")