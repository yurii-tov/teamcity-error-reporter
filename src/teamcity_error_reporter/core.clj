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
      "* %s\n%s\n** stacktrace\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n** stdout\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE"
      display-name
      (if artifact-path
        (format "[[file:%s][screenshot]]"
                artifact-path)
        "no screenshots attached")
      stacktrace
      stdout))))
