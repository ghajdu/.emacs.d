;;; names.el --- Insert names in different cases

;;; Commentary:

;;; Code:

(require 's)

(defun names/insert (beg end)
  "Insert BEG END in different cases."
  (interactive "*r")
  (save-excursion
    (let* ((regionLines (buffer-substring beg end))
           (names (split-string regionLines)))
      (goto-char end)
      (push-mark)
      (insert (concat "\n\n" (names/create names))))))

(ert-deftest test-names/insert ()
  (should (equal (with-temp-buffer
                   (switch-to-buffer (current-buffer))
                   (insert "fooBarBaz")
                   (names/insert (point-min) (point-max))
                   (buffer-string))
                 "fooBarBaz\n\nfooBarBaz\nFooBarBaz\nfoobarbaz\nFOOBARBAZ\nfoo_bar_baz\nFOO_BAR_BAZ\nfoo-bar-baz")))

(defun s-snake-upcase (s) (s-join "_" (mapcar 'upcase (s-split-words s))))
(defun s-lowercase (s) (s-join "" (mapcar 'downcase (s-split-words s))))
(defun s-upcase (s) (s-join "" (mapcar 'upcase (s-split-words s))))

(defun names/create (names)
  "Create NAMES in different cases."
  (cl-labels ((create-row (f) (s-join " " (mapcar f names))))
    (s-join "\n" (list (create-row 's-lower-camel-case)
                       (create-row 's-upper-camel-case)
                       (create-row 's-lowercase)
                       (create-row 's-upcase)
                       (create-row 's-snake-case)
                       (create-row 's-snake-upcase)
                       (create-row 's-dashed-words)))))

(ert-deftest test-names/create ()
  (should (equal (names/create '("fooBarBaz")) "fooBarBaz\nFooBarBaz\nfoobarbaz\nFOOBARBAZ\nfoo_bar_baz\nFOO_BAR_BAZ\nfoo-bar-baz")))

(provide 'names)

;;; names.el ends here
