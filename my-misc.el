
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

;; Search replace mvn compile exec:java -Dusername=<username> -Dpassword=<password>
(fset 'mbj/username-password
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([home 19 60 117 115 101 114 110 97 109 101 62 67108896 left left left left left left left left left left backspace 101 105 52 53 55 55 19 60 112 97 115 115 119 111 114 100 62 67108896 backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace 67 108 97 114 97 50 51 52 53] 0 "%d")) arg)))

(defun mbj/names (beg end)
  "insert beg end in different cases"
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
