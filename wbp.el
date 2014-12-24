;; wbp.el -- Window, Buffer, Point

;; Copyright (C) 2014-2015 Silao Xu

;; Author: Silao Xu
;; URL: https://github.com/writecoffee/tslisp
;; Version: 0.1.0

;;; Commentary:

;; manipulate the emacs window, buffer, point like a boss

;;; Code:

(require 'cl)
(setq debug-on-error t)

(defun wbp:switch-to-previous-buffer ()
  "Switches between the last two buffers in current frame."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun wbp:switch-buffers-between-frames ()
  "Switches the buffers between the two last frames."
  (interactive)
  (let ((this-frame-buffer nil)
        (other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-paramter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameer nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))

(defun wbp:transpose-all-buffers (k)
  (concat "Transpose all current frames in clockwise order in k strides "
	  "without leaving the current frame.")
  (interactive "n")
  (message "Stride: %s" k)
  (let* ((cw (selected-window))
	 (fw cw)
	 (nw (next-window))
	 (my-windows (list cw)))
    (progn
      ;; list all windows
      (while (not (eq nw fw))
	(setq my-windows (cons nw my-windows))
	(setq nw (next-window nw)))
      ;; partition the whole array by (gcd n k ) sections and then rotate it
      (let* ((n (length my-windows))
	     (k (mod k n))
	     (m (gcd n k))
	     (i 0))
	(while (< i m)
	  (setq hold (window-buffer (nth (mod (+ i k) n) my-windows)))
	  (set-window-buffer (nth (mod (+ i k) n) my-windows)
			     (window-buffer (nth i my-windows)))
	  ;; rotate j-th position elements from section to section
	  (let ((j (mod (+ i k) n)))
	    (while (/= j i)
	      (setq twb (window-buffer (nth (mod (+ j k) n) my-windows)))
	      (set-window-buffer (nth (mod (+ j k) n) my-windows) hold)
	      (setq hold twb)
	      (setq j (mod (+ j k) n))))
	  (setq i (1+ i))))
      )))

(defvar wbp:positions ()
  "The stack of waypoints managed by wbp")

(defun wbp:push-way-point ()
  "Push the current position to the waypoints stack."
  (interactive)
  (set-variable 'wbp:positions
		(cons (list (buffer-name) (point))
		      wbp:positions)))

(defun wbp:pop-way-point ()
  "Pop a position from the waypoints stack."
  (interactive)
  (unless wbp:positions
    (error "No wayponts to pop!"))
  (let* ((address (car wbp:positions))
	 (buffer (car address))
	 (point (cadr address))
	 (rest (cdr wbp:positions)))
    (set-variable 'wbp:positions rest)
    (pop-to-buffer buffer)
    (goto-char point)))

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c C-p") 'wpb:push-way-point)
(global-set-key (kbd "C-c C-q") 'wpb:pop-way-point)
(global-set-key (kbd "C-c r") 'wpb:transpose-all-buffers)
(global-set-key (kbd "C-c p") 'wpb:switch-to-previous-buffer)
