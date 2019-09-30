(defun make-teamcity-test-report (build-type-id)
  (interactive "sbuildTypeId: ")
  (let ((buffer "*test-report*"))
    (make-process
     :buffer buffer
     :name "test-report"
     :command (list "lein" "run" build-type-id))
     (switch-to-buffer buffer)
     (org-mode)))


(defun make-teamcity-test-report-demo ()
  (interactive)
  (make-teamcity-test-report ":demo"))
