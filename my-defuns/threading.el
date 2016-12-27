;;; threading.el --- Threading macros

;;; Commentary:

;;; Code:

(defmacro -> (&rest body)
  "The thread-last macro.  Threads the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(ert-deftest test-threading-first ()
  (should (equal
           (-> ""
               (concat "a")
               (concat "b")
               (concat "c"))
           "abc")))

(defmacro ->> (&rest body)
  "The thread-first macro.  Threads the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(ert-deftest test->> ()
  (should (equal
           (->> ""
                (concat "a")
                (concat "b")
                (concat "c"))
           "cba")))

(defmacro as-> (&rest body)
  "The thread-as macro.  Threads the BODY."
  (let ((result (pop body))
        (name (pop body)))
    (dolist (form body result)
      (setq result (append (mapcar (lambda (el) (if (equal el name) result el)) form))))))

(ert-deftest test-first-as-> ()
  (should (equal
           (as-> "" v
                 (concat v "a")
                 (concat v "b")
                 (concat v "c"))
           "abc")))

(ert-deftest test-last-as-> ()
  (should (equal
           (as-> "" v
                 (concat "a" v)
                 (concat "b" v)
                 (concat "c" v))
           "cba")))

(ert-deftest test-mixed-as-> ()
  (should (equal
           (as-> "a" v
                 (concat v "1" v "2" v "3" v))
           "a1a2a3a")))

;;; threading.el ends here
