;;; setup-helper.el --- Some custom emacs functions

;;; Commentary:

;; https://github.com/magnars/.emacs.d/blob/5ff65739ebda23cfeffa6f70a3c7ecf49b6154ae/defuns/editing-defuns.el

;;; Code:

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current 'region-beginning' and
'region-end' is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    ;; (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

;; comment current line
;;http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position))
      )
    (comment-or-uncomment-region beg end)
    (forward-line)))


;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines

(defun copy-to-end-of-line ()
  "Copy line till the end."
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as ARG prefix argument) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or ARG many lines as prefix argument."
  (interactive "P")
  (if (null arg)
      ;; (copy-to-end-of-line)
      (copy-whole-lines (prefix-numeric-value 1))
   (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  "This is bound to <keymap>M-w."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))


;; improve selective display, F5 toggles based on cursor
;; (global-set-key "\C-x$" 'set-selective-display-dlw)
(global-set-key (kbd "<f5>") 'set-selective-display-dlw)

(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset selective buffer by setting it to 0."
  (interactive "P")
  ;; (princ selective-display)
  (if (eq selective-display (1+(current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))


;; key bindings
;; https://github.com/magnars/.emacs.d/blob/master/settings/key-bindings.el
;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
;;(global-set-key (kbd "s-w") 'save-region-or-current-line)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(provide 'setup-helper)
;;; setup-helper.el ends here
