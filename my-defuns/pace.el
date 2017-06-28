;;; pace.el --- Pace

;;; Commentary:

;;; Code:

(defun pace (h m s km)
  "Calculates pace given H, M, S and KM."
  (let* ((totalSeconds (+ (* h 3600) (* m 60) s))
         (minutes (truncate (/ totalSeconds km 60)))
         (seconds (round (mod (/ totalSeconds (* km 1.0)) 60))))
    (format "%d:%02d min/km" minutes seconds)))

(ert-deftest test-pace ()
  (should (equal (pace 1 0 0 12) "5:00 min/km"))
  (should (equal (pace 0 44 4 10) "4:24 min/km"))
  (should (equal (pace 0 43 0 10) "4:18 min/km")))

(defun stride-length (minutes seconds cadence)
  (let ((l (/ 1000 (* cadence (+ minutes (/ seconds 60.0))))))
    (format "%.2f m/stride" l)))

(ert-deftest test-stride-length ()
  (should (equal (stride-length 5 0 180) "1.11 m/stride"))
  (should (equal (stride-length 4 30 180) "1.23 m/stride"))
  (should (equal (stride-length 4 0 180) "1.39 m/stride"))
  (should (equal (stride-length 4 0 170) "1.47 m/stride"))
  (should (equal (stride-length 3 28 180) "1.60 m/stride")))

(provide 'pace)
;;; pace.el ends here
