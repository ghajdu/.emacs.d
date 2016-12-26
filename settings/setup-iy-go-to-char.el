(require 'iy-go-to-char)

;; To make `iy-go-to-char' works better with `multiple-cursors', add
;; `iy-go-to-char-start-pos' to `mc/cursor-specific-vars' when mc is loaded:
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

(provide 'setup-iy-go-to-char)
