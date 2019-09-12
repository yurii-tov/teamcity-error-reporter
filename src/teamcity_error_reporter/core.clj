(ns teamcity-error-reporter.core
  (:import java.util.UUID)
  (:require (teamcity-error-reporter
             [parser :refer :all]
             [teamcity :refer :all])
            [clojure.java.io :as io]))


(defn parse-log
  [build]
  (let [artifacts (-> build
                      build-artifacts
                      artifacts-map)
        log-url (build-log-url build)]
    (with-open [r (io/reader log-url)]
      (extract-errors (line-seq r)
                      artifacts))))


(defn print-log-org-mode
  [log-parsed]
  (doseq [{:keys [display-name stacktrace stdout artifact-href]} log-parsed
          :let [artifact-file-name (second (re-find #"/([^/]+$)" artifact-href))
                artifact-path (io/file (str (System/getenv "tmp")
                                            "/"
                                            (UUID/randomUUID))
                                       artifact-file-name)]]
    (io/make-parents artifact-path)
    (with-open [in (io/input-stream artifact-href)
                out (io/output-stream artifact-path)]
      (io/copy in out))
    (println
     (format
      "* %s\n[[file:%s][screenshot]]\n** stacktrace\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n** stdout\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE"
      display-name
      artifact-path
      stacktrace
      stdout))))
