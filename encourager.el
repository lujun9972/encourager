;; -*- lexical-binding: t; -*-

(defgroup encourager nil
  "")

(defcustom encourager-buffer "*encourager"
  " Buffer used to display encourager-image"
  :group 'encourager)

(defcustom encourager-image-file (concat (file-name-directory buffer-file-name)  "dancing.gif")
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
