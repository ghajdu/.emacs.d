;;; my-misc.el --- Miscanellous functions

;;; Commentary:

;;; Code:

;; Open drag and drop in same window/instance
(if (fboundp 'ns-find-file)
    (global-set-key [ns-drag-file] 'ns-find-file))
(setq ns-pop-up-frames nil)

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name))))

;; When you press x, you are prompted whether or not to delete all the
;; marked files. With the top setting (default), you're prompted for
;; each directory individually, and with the always setting you're not
;; prompted (other than the initial prompt).
(defvar dired-recursive-deletes)
(setq dired-recursive-deletes 'always)

(defun mbj/downcase-char-at-point (n)
  "Downcase char at point N."
  (interactive "p")
  (dotimes (i (or n 1))
    (downcase-region (point) (progn (forward-char) (point)))))

(defun mbj/upcase-char-at-point (n)
  "Upcase char at point N."
  (interactive "p")
  (dotimes (i (or n 1))
    (upcase-region (point) (progn (forward-char) (point)))))

(defun ert-t ()
  "Run ert t."
  (interactive)
  (ert t))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (delete-other-windows))

;; Modify below vars as suits your needs
(defvar file-visiting-scratch-buffer-dir (expand-file-name "fscratch" "~/.emacs.d"))
(defvar file-visiting-scratch-buffer-pattern "^fscratch[1-9]*$")

(defun create-file-visiting-scratch-buffer nil
  "Create a new file visiting scratch buffer to work in. (could be fscratch - fscratchX)"
  (interactive)
  (unless (file-directory-p file-visiting-scratch-buffer-dir)
    (make-directory file-visiting-scratch-buffer-dir))
  (let ((n (length (directory-files file-visiting-scratch-buffer-dir '() file-visiting-scratch-buffer-pattern)))
        filename)
    (while (progn
             (setq filename (concat "fscratch"
                                    (if (= n 0) "" (int-to-string n))))
             (setq n (1+ n))
             (get-buffer filename)))
    (find-file (expand-file-name filename file-visiting-scratch-buffer-dir))
    (emacs-lisp-mode)))

(defun delete-file-visiting-scratch-buffers nil
  "Delete all file visiting scratch buffers"
  (interactive)
  (let ((n (length (directory-files file-visiting-scratch-buffer-dir '() file-visiting-scratch-buffer-pattern)))
        filename)
    (if (yes-or-no-p "Kill and delete all file visiting scratch buffers? ")
        (progn
          (dolist (buffer (buffer-list))
            (if (string-match-p file-visiting-scratch-buffer-pattern (buffer-name buffer))
                (kill-buffer buffer)))
          (delete-directory file-visiting-scratch-buffer-dir t nil)
          (message "Deleted %s" file-visiting-scratch-buffer-dir)))))
 
(provide 'my-misc)

;;; my-misc.el ends here
