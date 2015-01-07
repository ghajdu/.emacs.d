
;; Open drag and drop in same window/instance
(if (fboundp 'ns-find-file)
    (global-set-key [ns-drag-file] 'ns-find-file))
(setq ns-pop-up-frames nil)

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name))))

;; When you press x, you are prompted whether or not to delete all the
;; marked files. With the top setting (default), you're prompted for
;; each directory individually, and with the always setting you're not
;; prompted (other than the initial prompt).
(setq dired-recursive-deletes 'always)

(provide 'my-misc)
