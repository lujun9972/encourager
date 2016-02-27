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

(defcustom encourager-media-player-proc-name "*encourager-media*"
  ""
  :group 'encourager
  :type '(file :must-match t))

(defcustom encourager-media-file (concat (file-name-directory buffer-file-name) "夕山谣.mp4")
  ""
  :group 'encourager
  :type '(file :must-match t))


(defun encourager--play-media-in-loop (media-file)
  "play MEDIA-FILE in loop"
  (unless (file-exists-p media-file)
    (error "%s does not exist!" media-file))
  (start-process encourager-media-player-proc-name nil "mplayer" "--ontop" "--loop=0" media-file))


;;;###autoload
(defun encourager-enable ()
  (interactive)
  (with-auto-pause 10
    (encourager--play-media-in-loop encourager-media-file)))
;;;###autoload
(defun encourager-disable ()
  (interactive)
  (when (buffer-live-p (get-buffer encourager-buffer))
    (kill-buffer encourager-buffer))
  (delete-process (get-process encourager-media-player-proc-name)))

(provide 'encourager)


;;; encourager.el ends here
