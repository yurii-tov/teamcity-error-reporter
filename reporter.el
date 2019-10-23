(eval-when-compile (require 'subr-x))


(defconst teamcity-test-reporter/wd
  (file-name-directory load-file-name))


(defun make-teamcity-test-report (args)
  (interactive "sargs: ")
  (let* ((default-directory teamcity-test-reporter/wd)
         (command (string-join (append '("lein" "trampoline" "run")
                                       (split-string args))
                               " "))
         (report-path (string-trim-right
                       (shell-command-to-string command))))
    (find-file report-path)))


(defun make-teamcity-test-report-demo ()
  (interactive)
  (make-teamcity-test-report ":demo"))
