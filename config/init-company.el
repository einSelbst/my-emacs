;;; #init-company.el

(use-package company
  :commands company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-backends (delete 'company-bbdb company-backends)
        company-backends (delete 'company-dabbrev company-backends)
        company-backends (delete 'company-clang company-backends)
        company-backends (delete 'company-eclim company-backends)
        company-backends (delete 'company-oddmuse company-backends)
        company-backends (delete 'company-semantic company-backends)
        company-backends (delete 'company-xcode company-backends)
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-show-numbers 'left
        ;; global-company-mode 1
        ;; global-set-key (kbd "C-<tab>") 'company-complete
        ))

(provide 'init-company)
;;; init-company.el ends here.
