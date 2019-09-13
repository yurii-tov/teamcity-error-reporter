(ns teamcity-error-reporter.core
  (:import java.util.UUID)
  (:require (teamcity-error-reporter
             [parser :refer :all]
             [teamcity :refer :all])
            [clojure.java.io :as io]))


(defn parse-log
  "Fetch all resources related to given build (build log text file, artifacts info),
  and produce structured view of test failures.
  Argument: build - parsed xml structure obtained from teamcity api"
  [build]
  (let [artifacts (-> build
                      build-artifacts
                      artifacts-map)
        log-url (build-log-url build)]
    (with-open [r (io/reader log-url)]
      (extract-errors (line-seq r)
                      artifacts))))


(defn print-log-org-mode
  "Consume errors data, fetch artifacts, print org-mode document into stdout"
  [log-parsed]
  (doseq [{:keys [display-name message stacktrace stdout artifact-href]} log-parsed
          :let [artifact-file-name (when artifact-href
                                     (second (re-find #"/([^/]+$)" artifact-href)))
                artifact-path (and artifact-file-name
                                   (io/file (str (System/getenv "tmp")
                                                 "/"
                                                 (UUID/randomUUID))
                                            artifact-file-name))]]
    (when artifact-path
      (io/make-parents artifact-path)
      (with-open [in (io/input-stream artifact-href)
                  out (io/output-stream artifact-path)]
        (io/copy in out)))
    (println
     (format
      "* %s\n%s\n\n%s\n\n** stacktrace\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n** stdout\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE"
      display-name
      message
      (if artifact-path
        (format "[[file:%s][screenshot]]"
                artifact-path)
        "no screenshots attached")
      stacktrace
      stdout))))


(defn demo
  "Create report based on local example"
  []
  (let [test-log-lines
        (with-open [r (io/reader "example.log")]
          (vec (line-seq r)))
        test-errors (extract-errors test-log-lines {})]
    (print-log-org-mode test-errors)))


(defn -main
  "At this time, support only buildType argument, create report only for last build.
  If pass :demo, create test report based on local file"
  [& args]
  (if (= (first args) ":demo")
    (demo)
    (print-log-org-mode
     (-> (get-build (first args))
         parse-log
         print-log-org-mode))))
