;;; encourager.el --- atom-miku like plugin  -*- lexical-binding: t; -*-

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

;; encourager is an atom-miku like plugin

;;; Code:

(defgroup encourager nil
  "")

(defcustom encourager-buffer "*encourager"
  " Buffer used to display encourager-image"
  :group 'encourager
  :type 'string)

(defcustom encourager-image-file (concat (file-name-directory buffer-file-name)  "miku.gif")
  "Multi-frame image file to be displayed as encourager"
  :group 'encourager
  :type '(file :must-match t))

(defun encourager--image-show-next-frame (&optional image max-frame)
  "Show next frame of IMAGE. The frame will not exceed MAX-FRAME"
  (let* ((image (or image (get-char-property (point-min) 'display
                                             (or (get-buffer encourager-buffer)
                                                 (error "no encourager buffer found")))))
         (max-frame (or max-frame
                        (car (image-multi-frame-p image))))
         (current-frame (image-current-frame image)))
    (when max-frame
      (let* ((next-frame (mod (+ 1 current-frame)
                             max-frame)))
        (image-show-frame image next-frame t)))))

(defun encourager-enable ()
  (interactive)
  (let ((image (create-image encourager-image-file)))
    (when image
      (with-selected-window (display-buffer (get-buffer-create encourager-buffer))
        (erase-buffer)
        (insert-image image))
      (add-hook 'post-self-insert-hook #'encourager--image-show-next-frame))))

(defun encourager-disable ()
  (interactive)
  (remove-hook 'post-self-insert-hook #'encourager--image-show-next-frame)
  (when (buffer-live-p (get-buffer encourager-buffer))
    (kill-buffer encourager-buffer)))

(provide 'encourager)
