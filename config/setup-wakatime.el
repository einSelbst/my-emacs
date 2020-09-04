;;; setup-wakatime.el --- Emacs Minor Mode for WakaTime -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-10 12:37:51 einselbst>

;;; Commentary:
;; an automatic time tracking extension for Emacs using WakaTime.
;; Copyright (C) 2020 einSelbst
;; Author: einSelbst <einselbst@mailbox.org>

;; Wakatime configuration

;; wakatime-mode: Emacs Minor Mode for WakaTime
;; https://github.com/wakatime/wakatime-mode

;;; Code:
(require 'use-package)

(use-package wakatime-mode
  :if (executable-find "wakatime")
  :defer 4
  :config (progn (setq wakatime-cli-path (executable-find "wakatime"))
                   (global-wakatime-mode 1)))

;; (use-package wakatime-mode
;;   :init
;;   (add-hook 'prog-mode-hook 'wakatime-mode)
;;   :config (progn (setq wakatime-cli-path "/usr/local/bin/wakatime")
;;                  (setq wakatime-python-bin nil)
;;                  (global-wakatime-mode)))

;; (use-package wakatime-mode
;;   :hook (prog-mode . wakatime-mode)
;;   :config
;;   (setq wakatime-cli-path (executable-find "wakatime")
;;         wakatime-api-key "3fd63845-ecde-47ea-bd1a-7042221d1046")
;;   (defun wakatime-dashboard ()
;;     (interactive)
;;     (browse-url "https://wakatime.com/dashboard"))
;;   :general
;;   (tyrant-def "aw" 'wakatime-dashboard))

;; (use-package wakatime-mode

;;   :if (and (not noninteractive) (not degrade-p-minimalism) (executable-find* "wakatime"))
;;   :commands (wakatime-mode global-wakatime-mode)
;;   :diminish (wakatime-mode . "")
;;   :defer 4
;;   :init
;;   (progn
;;     (setq wakatime-disable-on-error t)
;;     ;; (setq wakatime-cli-path "~/.opt/wakatime/wakatime-cli.py")
;;     )
;;   :config
;;   (progn
;;     (when (executable-find "wakatime")
;;       (global-wakatime-mode 1))))

(provide 'setup-wakatime)
;;; setup-wakatime.el ends here
