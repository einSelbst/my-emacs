;;; setup-multiple-cursors.el --- Emacs Minor Mode for Multiple-Cursors -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-10 12:37:51 einselbst>

;;; Commentary:
;; Multiple cursors for Emacs.
;; https://github.com/magnars/multiple-cursors.el

;; https://www.youtube.com/watch?v=ozrGXYQIBHg
;; https://www.reddit.com/r/emacs/comments/iu0euj/getting_modern_multiple_cursors_in_emacs/

;; Disable cursors
  ;; - C-g: First press unmarks regions. Second press disables mc. (M-x mc/keyboard-quit)
  ;; - RET: Disable multiple cursors.
  ;; - To insert a newline, use (M-x electric-newline-and-maybe-indent) (C-j C-i C-n)

;; Insert numbers
  ;; - C-M-y (M-x mc/insert-numbers)
  ;; - Use prefix arg to start from a different number. (C-u-1-0)
  ;; - Optionally use (M-x rectangle-number-lines) (C-x r N)

;; More comments
  ;; - Use C-' (M-x mc-hide-unmatched-lines-mode) to hide unmatched lines. It cycles.
  ;; - Select all tags in web-mode, [tailwind.njk]: (M-x mc/mark-all-dwim).
  ;; - mc remembers how to apply multiple commands. [.mc-lists.el]
  ;; - Use (M-x just-one-space) (C-j C-i C-SPC) to add space at end of line. Use key binding.

;;; Code:
(require 'use-package)

(use-package multiple-cursors
  :bind (
          ;; Do What I Mean
         ("C-M-j" . mc/mark-all-dwim) ; both marked and unmarked region. multiple presses.
  
         ;; Expand region. (Also from Magnar Sveen)
         (global-set-key (kbd "C-M-l") 'er/expand-region) ; only type once, then l, -, 0
         
         ;; For continuous lines: Mark lines, then create cursors. Can be mid-line.
         ("C-M-c" . mc/edit-lines)
         ;; ("C-c C-SPC" . mc/edit-lines)

         ;; Select region first, then create cursors.
         ("C-M-/" . mc/mark-all-like-this) ; select text first. finds all occurrences.
         ;; ("C-c C->" . mc/mark-all-like-this)
         ("C-M-." . mc/mark-next-like-this)
         ;; ("C->" . mc/mark-next-like-this)
         ("C-M-," . mc/mark-previous-like-this)
         ;; ("C-<" . mc/mark-previous-like-this)

         ;; Skip this match and move to next one. (Note YouTube won't allow angle brackets here.)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)

         ;; ("H-SPC" . set-rectangular-region-anchor)
         ;; ("C-M-SPC" . set-rectangular-region-anchor)
         )
  )

(provide 'setup-multiple-cursors)
;;; setup-multiple-cursors.el ends here
