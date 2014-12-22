;;;; br0ns' emacs configuration

;; Enable backtrace when errors occur
;; (setq debug-on-error t)

;; based on https://github.com/magnars/.emacs.d

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Set path to configuration
(setq setup-dir
      (expand-file-name "setup" user-emacs-directory))

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Keep all the damn files emacs creates elsewhere
(setq emacs-file-dir
      (expand-file-name "~/.emacs.files"))

;; Set up load path
(add-to-list 'load-path setup-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'setup-misc)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-ido)
(require 'setup-hippie)
(require 'setup-bindings)
(require 'setup-shell)
