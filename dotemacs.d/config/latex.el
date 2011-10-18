(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode t)
            (reftex-mode t)
            (tex-pdf-mode t)
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            )
          )

(add-hook 'LaTeX-language-dk-hook
          (function (lambda () (ispell-change-dictionary "british")
                      )
                    )
          )