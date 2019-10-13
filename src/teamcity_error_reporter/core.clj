(ns teamcity-error-reporter.core
  (:import java.time.LocalDateTime
           java.time.format.DateTimeFormatter)
  (:require (teamcity-error-reporter
             [parser :refer :all]
             [teamcity :refer :all])
            [clojure.java.io :as io]
            [clojure.string :as cstr]))


(defn parse-log
  "Fetch all resources related to given build (build log text file, artifacts info),
  and produce structured view of test failures.
  Argument: build - parsed xml structure obtained from teamcity api"
  [build]
  (let [artifacts (-> build
                      build-artifacts
                      artifacts-map)
        log-url (build-log-url build)
        errors    (with-open [r (io/reader log-url)]
                    (extract-errors (line-seq r)
                                    artifacts))
        properties (build-properties build)]
    {:properties properties
     :errors errors}))


(defn gen-report-org-mode
  "Consume parsed-log output, fetch artifacts, print org-mode document into file"
  [{{:keys [url number build-type-id status]} :properties errors :errors}]
  (let [report-id (let [d (LocalDateTime/now)
                        f (DateTimeFormatter/ofPattern "yyyyMMdd_HHmmss")]
                    (.format d f))
        report-dir (str (System/getenv "tmp") "/" report-id)
        report-file (io/file report-dir "test-report.org")]
    ;; create report directory
    (io/make-parents report-file)
    ;; write report
    (with-open [w (io/writer report-file)]
      (binding [*out* w]
        ;; properties
        (print                              
         (format "#+TITLE:%s %s\n\n%s\n\n%s\n\n"
                 build-type-id                
                 number                       
                 status                       
                 url))                        
        ;; errors
        (doseq [{:keys [display-name message stacktrace stdout artifact-href]} errors
                :let [artifact-file-name (when artifact-href
                                           (second (re-find #"/([^/]+$)" artifact-href)))
                      artifact-path (and artifact-file-name
                                         (io/file report-dir
                                                  artifact-file-name))]]
          (when artifact-path
            (with-open [in (io/input-stream artifact-href)
                        out (io/output-stream artifact-path)]
              (io/copy in out)))
          (print
           (format
            "* %s\n** summary\n%s\n\n%s\n\n** stacktrace\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE%s\n"
            display-name
            message
            (if artifact-path
              (format "[[file:%s][screenshot]]"
                      artifact-file-name)
              "no screenshots attached")
            stacktrace
            (str (when stdout
                   (format "\n** stdout\n#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE" stdout))))))))
    ;; write path to report to stdout
    (println (cstr/replace (.getAbsolutePath report-file) "\\" "/"))))
                           


(defn demo
  "Create report based on local example"
  []
  (let [test-log-lines
        (with-open [r (io/reader "example.log")]
          (vec (line-seq r)))
        test-errors (extract-errors
                     test-log-lines
                     (zipmap (map (fn [{:keys [stdout]}] (when stdout (extract-screenshot-name stdout)))
                                  (extract-errors test-log-lines {}))
                             (repeatedly (fn [] (format "https://picsum.photos/id/%d/200/300.jpg"
                                                        (inc (rand-int 500)))))))]
    (gen-report-org-mode
     {:errors test-errors
      :properties {:url
                   "http://example.teamcity.server/viewLog.html?buildId=27749&buildTypeId=e5_selenium",
                   :status "Tests failed: 4 (1 new), passed: 535, ignored: 1",
                   :id "27749",
                   :build-type-id "e5_selenium",
                   :number "1240"}})))


(defn -main
  "At this time, support only buildType argument, create report only for last build.
  If pass :demo, create test report based on local file"
  [& args]
  (if (= (first args) ":demo")
    (demo)
    (-> (apply get-build
               (map (fn [a] (if (cstr/starts-with? a ":")
                              (read-string a)
                              a))
                    args))
        parse-log
        gen-report-org-mode)))
