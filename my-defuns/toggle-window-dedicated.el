;;; toggle-window-dedicated.el --- Control whether or not Emacs is allowed to display another buffer in current window.

;;; Commentary:

;;; Code:

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(provide 'toggle-window-dedicated)

;;; toggle-window-dedicated.el ends here
