;;; env.el --- Transforms env variables

;;; Commentary:

;;; Code:

(defun env/windows-to-unix-vars (begin end)
  "Transforms java %foo% %bar% Baz to java $foo $bar Baz from BEGIN to END."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "%\\([^[:space:]|[:cntrl:]]+\\)%" end t)
      (replace-match "$\\1"))))

(ert-deftest test-env/windows-to-unix-vars ()
  (should (equal
           (with-temp-buffer
             (insert "qwerty\n%foo-bar%\n%baz%qwerty")
             (env/windows-to-unix-vars (point-min) (point-max))
             (buffer-string))
           "qwerty\n$foo-bar\n$bazqwerty")))

(defun env/unix-to-windows-vars (begin end)
  "Transforms java $foo $bar Baz to java %foo% %bar% Baz from BEGIN to END."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (and (<= (point) end) (re-search-forward "\\$\\([^[:space:]|[:cntrl:]]+\\)" end t))
      (replace-match "%\\1%"))))

(ert-deftest test-env/unix-to-windows-vars ()
  (should (equal
           (with-temp-buffer
             (insert "qwerty\n$foo-bar\n$baz qwerty")
             (env/unix-to-windows-vars (point-min) (point-max))
             (buffer-string))
           "qwerty\n%foo-bar%\n%baz% qwerty")))

(provide 'env)

;;; env.el ends here
