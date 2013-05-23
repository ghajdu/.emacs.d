.emacs.d
========

To use this configuration:

1. Create a file: ~/.emacs.d/init.el
2. With the following content:

   (setq emacs.d-directory (expand-file-name ".emacs.d" "<path to the parent of this directory, e.g. /Users/magnus/Dropbox/emacs>"))
   (load (expand-file-name "init.el" emacs.d-directory))

