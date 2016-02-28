(defun SH-hook ()
  (define-key sh-mode-map [(ctrl return)] #'SH-SH)
  (define-key sh-mode-map [(meta return)] #'SH-SHnew))
(add-hook 'sh-mode-hook 'SH-hook)

(defun SH-SH (&optional NEW)
  "h"
  (interactive)
  (unless (eq (count-windows) 2)
    (delete-other-windows)
    (split-window-right))
  (let (beg1 end1 toSend1 cb1 p1)
    (when (region-active-p)
      (set 'beg1 (region-beginning))
      (set 'end1 (region-end)))
    (unless (region-active-p)
      (beginning-of-line)
      (set 'beg1 (point))
      (end-of-line)
      (set 'end1 (point)))
    (set 'toSend1 (buffer-substring-no-properties beg1 end1))
    (unless (string-suffix-p "\n" toSend1)
      (set 'toSend1 (concat toSend1 "\n")))
    (set 'cb1 (current-buffer))
    (set 'p1 (SH-get-shell-process NEW))
     (display-buffer-use-some-window (process-buffer p1) '(("side" . "right")))
    (with-current-buffer (process-buffer p1)
      (goto-char (point-max))
      (insert toSend1)
      (set-marker (process-mark p1) (point-max))
      (process-send-string p1 toSend1)
      (goto-char (point-max)))
    (switch-to-buffer cb1)
    (ess-next-code-line)))

(defun SH-SHnew ()
  "h"
  (interactive)
  (SH-SH t))

(defun SH-get-shell-process (&optional NEW)
  "h"
  (interactive)
  ;; p1 = process to return
  ;; lShProc = list of shell processes
  (let (p1
	lShProc)
    (when NEW
      (set 'p1 (SH-new-shell-process)))
    (unless p1
      (mapc (lambda (x)
	      (when (string-match "shell" (process-name x))
		(push x lShProc)))
	    (process-list))
      (set 'p1
	   (car
	    (sort lShProc #'SH-X-more-recent))))
    (unless p1
      (set 'p1 (SH-new-shell-process)))
    p1))

(defun SH-new-shell-process ()
  "h"
  (interactive)
  (let (p1)
  (set 'p1 (get-buffer-process
	    (shell
	     (generate-new-buffer "*shell*"))))
  p1))

(defun SH-X-more-recent (X Y)
  "
Returns t if the `process-buffer' associated with X is higher on the `buffer-list' than that for Y.
That is, X has more recently been active (display or selected)."
  (interactive)
  (<
   (cl-position (process-buffer X) (buffer-list))
   (cl-position (process-buffer Y) (buffer-list))))

;; (defun SH-newer-process (x y)
;;   "h"
;;   (interactive)
;;   (time-less-p
;;    (cdr (assoc 'start (process-attributes (process-id y))))
;;    (cdr (assoc 'start (process-attributes (process-id x))))))

;; function taken from ess package
(defun ess-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.	 Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))


;; (setq shelllist (process-shell))
;; (setq shelln (length shelllist))
;; (if (eq shelln 0)
;;     (progn (shell)
;; 	   (switch-to-buffer cbuf)
;; 	   (setq outpr (get-process "shell"))
;; 	   (sleep-for 0.5)))
;; (if (eq shelln 1)
;;     (setq outpr (get-process (elt shelllist 0))))
;; (if (> shelln 1)
;; (progn
;; (setq proc (completing-read "Send code to:" shelllist nil t (elt shelllist 0)))
;; (setq outpr (get-process proc))))
;; outpr)

(provide 'SH)
