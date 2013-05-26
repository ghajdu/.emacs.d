;; Open drag and drop in same window/instance
(if (fboundp 'ns-find-file)
    (global-set-key [ns-drag-file] 'ns-find-file))
(setq ns-pop-up-frames nil)

(provide 'my-misc)
