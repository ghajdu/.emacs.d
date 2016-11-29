
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
  (dotimes (el (if n n 1)) 
    (downcase-region (point) (progn (forward-char) (point)))))

(defun mbj/upcase-char-at-point (n)
  "Upcase char at point."
  (interactive "p")
  (dotimes (el (if n n 1)) 
    (upcase-region (point) (progn (forward-char) (point)))))

;; Transforms java %foo% %bar% Baz to java $foo $bar Baz
(fset 'mbj/windows-to-unix-vars
      [home ?\M-& ?% ?\( ?\[ ?^ ?% ?\] ?+ ?% backspace ?\) ?% return ?$ ?\\ ?1 return ?!])

;; Transforms java $foo $bar Baz to java %foo% %bar% Baz
(fset 'mbj/unix-to-windows-vars
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([home 134217766 36 40 91 97 45 122 65 45 90 48 45 57 93 43 41 1 92 5 return 37 92 49 37 return 33 24 11 110 109 98 106 47 117 110 105 120 45 116 111 45 119 105 110 100 111 119 115 45 118 97 114 115] 0 "%d")) arg)))

(defun mbj/names (beg end)
  "Insert beg end in different cases."
  (interactive "*r")
   (defun s (names)
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
 (save-excursion
   (let* ((regionLines (buffer-substring beg end))
          (names (split-string regionLines)))
     (goto-char (region-end))
     (push-mark)
     (insert (concat "\n\n" (s names))))))

(provide 'my-misc)
