(eval-when-compile (require 'subr-x))

(defun make-teamcity-test-report (args)
  (interactive "sargs: ")
  (let* ((command (string-join (append '("lein" "run")
                                       (split-string args))
                               " "))
         (report-path (string-trim-right
                       (shell-command-to-string command))))
    (find-file report-path)))
  

(defun make-teamcity-test-report-demo ()
  (interactive)
  (make-teamcity-test-report ":demo"))
