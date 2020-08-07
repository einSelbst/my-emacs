;;; init-windmove.el --- let me easily switch buffers
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package windmove
  :init
  (windmove-default-keybindings)
  :config
  ;; use command key on Mac
  ;; (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  ;; (general-define-key
  ;;  "<left>" 'windmove-left
  ;;  "<right>" 'windmove-right
  ;;  "<up>" 'windmove-up
  ;;  "<down>" 'windmove-down
  ;;  )
  )

;; https://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/
;; automatically save buffers associated with files on buffer switch
;; and on windows switch
;; (defadvice switch-to-buffer (before save-buffer-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
;; (defadvice other-window (before other-window-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
;; (defadvice windmove-up (before other-window-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
;; (defadvice windmove-down (before other-window-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
;; (defadvice windmove-left (before other-window-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
;; (defadvice windmove-right (before other-window-now activate)
;;   (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))


;; saves the buffers if I de-focus emacs completely (moving to another screen)
;; (add-hook 'focus-out-hook
;; (add-hook 'after-focus-change-hook
          ;; (save-some-buffers t))

;; (add-hook 'after-focus-change-hook
;;           (lambda () (flet ((message
;;                              (format &rest args) nil))
;;                        (save-some-buffers t))))

(provide 'init-windmove)
;;; init-windmove.el ends here
