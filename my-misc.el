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

(defun mbj/downcase-char-at-point (n)
  "Downcase char at point."
  (interactive "p")
  (dotimes (i (or n 1)) 
    (downcase-region (point) (progn (forward-char) (point)))))

(defun mbj/upcase-char-at-point (n)
  "Upcase char at point."
  (interactive "p")
  (dotimes (i (or n 1)) 
    (upcase-region (point) (progn (forward-char) (point)))))

(defun mbj/windows-to-unix-vars (begin end)
  "Transforms java %foo% %bar% Baz to java $foo $bar Baz"
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "%\\([^[:space:]|[:cntrl:]]+\\)%" end t)
      (replace-match "$\\1"))))

(defun mbj/unix-to-windows-vars (begin end)
  "Transforms java $foo $bar Baz to java %foo% %bar% Baz"
  (interactive "r")
  (save-excursion
    (goto-char begin)    
    (while (re-search-forward "\\$\\([^[:space:]|[:cntrl:]]+\\)" end t)
      (replace-match "%\\1%"))))

(defun ert-t ()
  "Runs ert t"
  (interactive)
  (ert t))

(defun mbj/names-insert (beg end)
  "Insert beg end in different cases."
  (interactive "*r")
  (save-excursion
    (let* ((regionLines (buffer-substring beg end))
           (names (split-string regionLines)))
      (goto-char (region-end))
      (push-mark)
      (insert (concat "\n\n" (mbj/names names))))))

(defun mbj/names (names)
  (defun s-snake-upcase (s)
    (s-join "_" (mapcar 'upcase (s-split-words s))))
  (defun s-lowercase (s)
    (s-join "" (mapcar 'downcase (s-split-words s))))
  (defun s-upcase (s)
    (s-join "" (mapcar 'upcase (s-split-words s))))
  (defun create-row (f)
    (s-join " " (mapcar f names)))
  (s-join "\n" (list (create-row 's-lower-camel-case)
                     (create-row 's-upper-camel-case)
                     (create-row 's-lowercase)
                     (create-row 's-upcase)
                     (create-row 's-snake-case)
                     (create-row 's-snake-upcase)
                     (create-row 's-dashed-words))))


(provide 'my-misc)
