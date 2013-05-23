.emacs.d
========

To use this configuration:

1. git clone --recursive git://github.com/bjuvensjo/.emacs.d.git emacs.d
2. In directory emacs.d/site-lisp/tern, run npm install
2. Create a file: ~/.emacs.d/init.el
3. With the following content:

        (setq emacs.d-directory (expand-file-name "emacs.d" "<path to the grandparent of this directory,
            e.g. /Users/magnus/Dropbox/emacs>"))
        (load (expand-file-name "init.el" emacs.d-directory))

