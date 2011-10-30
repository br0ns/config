;; some people like to give files containing functors the ending .fun
(add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))

;; default indentation is four
(setq sml-indent-level 2)

;; use "mosml -P full" as default interpreter
(setq sml-program-name "mosml")
(setq sml-default-arg "-P full")

(setq sml-use-command
      (concat
       "local "
       "val filei = \"%s\" "
       "val fileo = filei ^ \".preml\" "
       "val _ = OS.Process.system (\"preml \\\"\" ^ filei ^ \"\\\"\") "
       "val _ = use fileo "
       "val _ = OS.FileSys.remove fileo "
       "in end")
      )

(defun my-sml-mode-hook () "Global defaults for SML mode"
  (define-key sml-mode-map (kbd "M-SPC") 'hippie-expand)
)
(add-hook 'sml-mode-hook 'my-sml-mode-hook)
