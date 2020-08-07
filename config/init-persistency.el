;;; init-persistency.el --- saving files and restoring sessions

;;; Commentary:

;; Files, Config, Buffers

;;; Code:
(require 'use-package)

;; remember open buffers
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#buffers-and-windows
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package desktop
  ;;:defer nil
;  :hook
;;  (after-init . desktop-read)
  ;; :config
  ;; (setq desktop-dirname "~/.emacs.d"
  ;;       desktop-base-file-name "desktop"
  ;;       desktop-base-lock-name "desktop.lock"
  ;;       desktop-restore-frames t
  ;;       desktop-restore-reuses-frames t
  ;;       desktop-restore-in-current-display t
  ;;       desktop-restore-forces-onscreen t)
  :custom
  ;;(desktop-restore-eager   1 "Restore the first buffer right away")
;;  (desktop-lazy-idle-delay 1 "Restore the other buffers 1 second later")
  (desktop-lazy-verbose  nil "Be silent about lazily opening buffers")
  :init
  (desktop-save-mode)
;;  (add-to-list 'desktop-globals-to-save 'golden-ratio-adjust-factor)
  )

;; save history
;; https://github.com/sboosali/.emacs.d/blob/master/sboo/configuration/10-internal-packages/sboo-savehist.el
;; https://github.com/fasciism/dot-emacs/blob/master/2017-01-13-remembering-history.org
(use-package savehist
  :config
  (setq-default savehist-file (concat user-emacs-directory "savehist"))
  (unless (file-exists-p savehist-file)
    (write-file savehist-file))
  (setq savehist-save-minibuffer-history t)
  (setq history-delete-duplicates t)
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25)
  :init
  (savehist-mode)
  ;; :hook
  ;; (after-init . savehist-mode)
  )


;; https://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/
;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))


;; saves the buffers if I de-focus emacs completely (moving to another screen)
(add-hook 'after-focus-change-hook
          (save-some-buffers t))

;; (add-hook 'after-focus-change-hook
;;           (lambda () (flet ((message
;;                              (format &rest args) nil))
;;                        (save-some-buffers t))))

;; this would be an alternative to the above simple hook
;; (use-package focus-autosave-mode
;;   :init (focus-autosave-mode)
;;   :diminish
;;   :commands focus-autosave-mode)

(provide 'init-persistency)
;;; init-persistency.el ends here
