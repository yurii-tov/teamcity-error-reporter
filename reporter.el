(eval-when-compile (require 'subr-x))


(defconst teamcity-test-reporter/wd
  (file-name-directory load-file-name))


(setq teamcity-test-reporter/last-args nil)


(defun teamcity-test-report (args)
  (interactive (list (read-from-minibuffer
                      (if teamcity-test-reporter/last-args
                          (format "args (default: %s): "
                                  teamcity-test-reporter/last-args)
                        "args: "))))
  (if (zerop (string-width args))
      (when teamcity-test-reporter/last-args
        (setq args teamcity-test-reporter/last-args))
    (setq teamcity-test-reporter/last-args args))
  (let* ((default-directory teamcity-test-reporter/wd)
         (command (string-join (append '("lein" "trampoline" "run")
                                       (split-string args))
                               " "))
         (report-path (string-trim-right
                       (shell-command-to-string command))))
    (find-file report-path)))


(defun teamcity-test-report-demo ()
  (interactive)
  (teamcity-test-report ":demo"))
