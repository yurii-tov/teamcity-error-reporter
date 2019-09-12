(defun make-teamcity-test-report ()
  (interactive)
  (let ((buffer "*selenium-test-report*"))
    (make-process
     :buffer buffer
     :name "test-report"
     :command '("lein" "run" "e5_selenium"))
    (switch-to-buffer buffer)
    (org-mode)))
