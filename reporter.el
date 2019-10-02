(defun make-teamcity-test-report (args)
  (interactive "sargs: ")
  (let ((buffer "*test-report*"))
    (make-process
     :buffer buffer
     :name "test-report"
     :command (append '("lein" "run")
                      (split-string args)))
     (switch-to-buffer buffer)
     (org-mode)))


(defun make-teamcity-test-report-demo ()
  (interactive)
  (make-teamcity-test-report ":demo"))
