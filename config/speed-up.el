;;; speed-up.el --- Speed up configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;; Speed up startup
;;; Code:
(message "Speed-up emacs startup")

;; (when (version< emacs-version "25.1")
;;   (error "This requires Emacs 25.1 and above!"))

;; startup time hacks

;; ----------- disable gc on startup -----------
(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes
;; (setq gc-cons-threshold 40000000)
;; (setq gc-cons-threshold 30000000)

;; reset it after load
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 1024 1024 20))))

;; ----------- empty file handler alist --------
(defvar cfg--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist cfg--file-name-handler-alist)))

(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %.2f seconds with %d garbage collections."
                         (float-time (time-subtract after-init-time before-init-time))
                         gcs-done)))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after startup."

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              ;; (add-hook 'focus-out-hook 'garbage-collect)
              (add-hook 'after-focus-change-function 'garbage-collect)
            )
            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold 40000000))

            (defun my-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

(provide 'speed-up)
;;; speed-up.el ends here
