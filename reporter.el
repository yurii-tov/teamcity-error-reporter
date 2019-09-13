(setq teamcity-test-report-command
      '("lein" "run" "e5_selenium"))
      

(defun make-teamcity-test-report ()
  (interactive)
  (let ((buffer "*test-report*"))
    (make-process
     :buffer buffer
     :name "test-report"
     :command teamcity-test-report-command)
    (switch-to-buffer buffer)
    (org-mode)))
