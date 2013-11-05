(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, bash, java.
File suffix is used to determine what program to run."
  (interactive)
  (let (ext-map file-name file-ext prog-name cmd-str)
    ;; get the file name
    ;; get the program name
    ;; run it
    (setq ext-map
          '(
            ("groovy" . "/usr/local/bin/groovy")
            ("coffee" . "/usr/local/share/npm/bin/coffee")
            ("litcoffee" . "/usr/local/share/npm/bin/coffee")
            ("java" . "javac")
            ("js" . "/usr/local/bin/node")
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("sh" . "bash")
            )
          )
    (setq file-name (buffer-file-name))
    (setq file-ext (file-name-extension file-name))
    (setq prog-name (cdr (assoc file-ext ext-map)))
    (setq cmd-str (concat prog-name " " "\"" file-name "\""))
    (shell-command cmd-str)))

(provide 'run-current-file)
