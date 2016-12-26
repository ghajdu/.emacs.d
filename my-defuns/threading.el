;;; threading.el --- Threading macros

;;; Commentary:

;;; Code:

(defmacro ->> (&rest body)
  "The thread-first macro.  Threads the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro -> (&rest body)
  "The thread-last macro.  Threads the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

;;; threading.el ends here
