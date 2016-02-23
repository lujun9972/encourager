;;; encourager.el --- encourage to use emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-01-06
;; Version: 0.1
;; Keywords: convenience, image
;; URL: https://github.com/lujun9972/encourager

;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; encourager's code can be found here:
;;   http://github.com/lujun9972/encourager

;;; Commentary:

;; encourager is an atom-miku like plugin. It will display a girl dancing while using emacs.
;; When you stopped using emacs, the girl stopped dancing either.

;;; Code:

(defgroup encourager nil
  "")

(defcustom encourager-buffer "*encourager*"
  " Buffer used to display encourager-image"
  :group 'encourager
  :type 'string)

(defcustom encourager-image-file (concat (file-name-directory buffer-file-name)  "dancing.gif")
  "Multi-frame image file to be displayed as encourager"
  :group 'encourager
  :type '(file :must-match t))


(defun encourager--get-image (encourager-buffer)
  "Return encourager image which displayed in ENCOURAGER-BUFFER"
  (get-char-property (point-min) 'display
                     (or (get-buffer encourager-buffer)
                         (error "no encourager buffer found"))))

(defun encourager--pause-image-animate (&optional image)
  "encourager--image"
  (let ((image (or image
                   (encourager--get-image encourager-buffer))))
    (cancel-timer (image-animate-timer image))))

(defun encourager--resume-image-animate (&optional image)
  "encourager--image"
  (let* ((image (or image
                    (encourager--get-image encourager-buffer)))
         (current-frame (image-current-frame image)))
    (image-animate image current-frame t)))

(defcustom encourager-music-player-proc-name "*encourager-music*"
  ""
  :group 'encourager
  :type '(file :must-match t))

(defcustom encourager-music-file (concat (file-name-directory buffer-file-name) "夕山谣.mp3")
  ""
  :group 'encourager
  :type '(file :must-match t))

(defun encourager--pause-music (&optional proc)
  ""
  (let ((proc (or proc
                  (get-process encourager-music-player-proc-name))))
    (when (processp proc) 
      (signal-process proc 'SIGSTOP))))

(defun encourager--resume-music (&optional proc)
  ""
  (let ((proc (or proc
                  (get-process encourager-music-player-proc-name))))
    (when (processp proc)
      (signal-process proc 'SIGCONT))))

(defun encourager--pause ()
  (encourager--pause-image-animate)
  (encourager--pause-music)
  (add-hook 'post-command-hook 'encourager--resume))

(defun encourager--resume (&optional delay-seconds)
  (let ((delay-seconds (or delay-seconds
                           10)))
    (encourager--resume-image-animate)
    (encourager--resume-music)
    (remove-hook 'post-command-hook #'encourager--resume)
    (run-with-idle-timer delay-seconds nil #'encourager--pause)))


(defun encourager--play-music-in-loop (music-file)
  "play MUSIC-FILE in loop"
  (unless (file-exists-p music-file)
    (error "%s does not exist!" music-file))
  (let ((proc (start-process encourager-music-player-proc-name nil "mpg123" "-q" music-file)))
    (set-process-sentinel proc (lambda (proc event)
                                 (when (eq 'exit (process-status proc))
                                   (set-process-sentinel  (start-process encourager-music-player-proc-name nil "mpg123" "-q" music-file)
                                                          (process-sentinel proc)))))))

;; (defun encourager--image-show-next-frame (&optional image max-frame)
;;   "Show next frame of IMAGE. The frame will not exceed MAX-FRAME"
;;   (let* ((image (or image (encourager--get-image encourager-buffer)))
;;          (max-frame (or max-frame
;;                         (car (image-multi-frame-p image))))
;;          (current-frame (image-current-frame image)))
;;     (when max-frame
;;       (let* ((next-frame (mod (+ 1 current-frame)
;;                               max-frame)))
;;         (image-show-frame image next-frame t)))))

;;;###autoload
(defun encourager-enable ()
  (interactive)
  (let ((image (create-image encourager-image-file)))
    (when (file-exists-p encourager-music-file)
      (encourager--play-music-in-loop encourager-music-file))
    (when image
      (with-selected-window (display-buffer (get-buffer-create encourager-buffer))
        (erase-buffer)
        (insert-image image))
      (encourager--resume))))
;;;###autoload
(defun encourager-disable ()
  (interactive)
  (encourager--pause)
  (when (buffer-live-p (get-buffer encourager-buffer))
    (kill-buffer encourager-buffer))
  (delete-process (get-process encourager-music-player-proc-name)))

(provide 'encourager)


;;; encourager.el ends here
