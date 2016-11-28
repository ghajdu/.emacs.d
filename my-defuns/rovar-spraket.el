(defun mbj/rövarspråket()
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (not (eobp))
      (when (member (following-char) '(?b ?c ?d ?f ?g ?h ?j ?k ?l ?m ?n ?p ?q ?r ?s ?t ?v ?w ?x ?z ?B ?C ?D ?F ?G ?H ?J ?K ?L ?M ?N ?P ?Q ?R ?S ?T ?V ?W ?X ?Z))
        (insert (string (following-char) ?o)))
      (forward-char 1))))

;;(global-set-key (kbd "C-c C-s") 'mbj/rövarspråket)

(provide 'rovar-spraket)



