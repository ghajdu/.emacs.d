(defun pace (h m s km)
  (let* ((totalSeconds (+ (* h 3600) (* m 60) s))
         (minutes (truncate (/ totalSeconds km 60)))
         (seconds (round (mod (/ totalSeconds (* km 1.0)) 60))))
    (concat (number-to-string minutes) ":" (number-to-string seconds) " min/km")))

(ert-deftest test-pace ()
  (should (equal (pace 1 0 0 12) "5:0 min/km"))
  (should (equal (pace 0 44 4 10) "4:24 min/km")))

(provide 'pace)
