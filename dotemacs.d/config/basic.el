; Uniquify
(require 'uniquify)

;; (require 'tramp)

(require 'mic-paren)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(require 'evil-numbers)

(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; Display time in mode line
(display-time)

;; Set title when window configuration changes
(add-hook 'window-configuration-change-hook 'set-title)

;; Rename new Eshell uniquely
(add-hook 'eshell-mode-hook
          '(lambda nil
             (rename-uniquely)
          )
)

;; Create dir if non-existing
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'after-load-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook
          '(lambda ()
             (message (concat
                       "Wrote "
                       (buffer-file-name)
                       " (and removed trailing whitespace)"))))

;; Color theme - Use color-theme-select to show themes
;; (color-theme-initialize)
;; (color-theme-taming-mr-arneson)

;; Line High lighting
(global-hl-line-mode t)
(set-face-background 'hl-line "Light Green")

(set-variable 'scroll-conservaly 5)

(transient-mark-mode 1)

(show-paren-mode 1)

(set-scroll-bar-mode nil)

(tool-bar-mode -1)

(tooltip-mode 1)

(menu-bar-mode -1)

(blink-cursor-mode -1)

(setq inhibit-startup-message t)

(set-default 'fill-column 80)

;; (icomplete-mode 1)

;; Tab width
(setq default-tab-width 2)

;; Column number in mode line
(setq column-number-mode t)

;; Don't use tabs dammit
(setq-default indent-tabs-mode nil)

;; Syntax colouring
(global-font-lock-mode t)

;; truncate lines if they are too long
(setq truncate-lines t)

;; trucate even even when screen is split into multiple windows
(setq truncate-partial-width-windows nil)

;; Default to text mode
(setq default-major-mode 'text-mode)

(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Load filladapt
(require 'filladapt)
(setq-default filladapt-mode t) ; Always use this

