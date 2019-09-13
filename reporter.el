(setq teamcity-test-report-args
      '("e5_selenium"))
      

(defun make-teamcity-test-report ()
  (interactive)
  (let ((buffer "*test-report*"))
    (make-process
     :buffer buffer
     :name "test-report"
     :command (concatenate 'list
                           '("lein" "run")
                           teamcity-test-report-args)
    (switch-to-buffer buffer)
    (org-mode))))


(defun make-teamcity-test-report-demo ()
  (interactive)
  (let ((teamcity-test-report-args '(":demo")))
    (make-teamcity-test-report)))
    
