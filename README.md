# .emacs.d

To use this configuration:

1. git clone --recursive git://github.com/bjuvensjo/.emacs.d.git emacs.d
1. cd emacs.d/site-lisp/tern, and execute npm install
1. Install markdown, e.g. brew install markdown
1. Create a file: ~/.emacs.d/init.el, with the content below

## init.el

    ;; To place the directory in a Dropbox (or equivalent) folder, is an easy way to share configuration across your computers
    (setq emacs.d-directory (expand-file-name "emacs.d" "<path to the parent of the emacs.d directory, e.g. ~/Dropbox/emacs>"))
    (load (expand-file-name "init.el" emacs.d-directory))

