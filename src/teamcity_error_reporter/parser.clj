(ns teamcity-error-reporter.parser
  "Teamcity build log parser"
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))


(defn message-type [sign]
  (case sign
    "F" :failure
    "W" :third-party
    :undefined))


(defn extract-message
  "Try to extract message from given string, return nil if string is not a message at all.
  Return hash map like {:type :failure
                        :source \"com.company.tests.MyTest.testSomething\"
                        :message \"Only local connections are allowed\"}"
  [string]
  (let [[_ sign source message :as match]
        (re-find #"^\[[^]]+\](.):\s+?\[(.+?)\]\s(.+)"
                 string)]
    (when match
      {:type (message-type sign)
       :source source
       :message message})))


(defn plain-message?
  "test if given line is an arbitrary output (i.e. not timsetamped)
   For example: [19:35:25] : Compute revision for 'Selenium 2' is not a plain message"
  [line]
  (not (re-matches #"^\[[^]]+].+" line)))


(defn stacktrace?
  "Test if given collection of strings represents stacktrace"
  [lines]
  (some (fn [l] (cstr/starts-with? l "\tat"))
        lines))


(defn stdout?
  "Test if given parsed message represents stdout"
  [{:keys [message]}]
  (cstr/includes? message "[Test Output]"))


(defn extract-screenshot-name
  "Try to extract screenshot name from given string, otherwise return nil"
  [string]
  (second (re-find #"value='(.+?.png)'" string)))


(defn extract-display-name
  "Get short name of a test.
  Example:
  'ru._1c.ui_tests.school.tests.old.library.ViewInOtherWindowTest.viewEor2011'
  becomes 'ViewInOtherWindowTest.viewEor2011'"
  [long-name]
  (->> long-name
       (re-find #"\.([^.]+\.[^.]+$)")
       second))


(defn extract-errors
  "Extract all error messages from log.
  Arguments:
  sequence of logfile's lines;
  hashmap of file-href (to be able to attach direct links)
  Return vector of items like {:test \"...\"
                               :display-name \"...\"
                               :message \"...\"
                               :stacktrace \"...\"
                               :stdout \"...\"
                               :artifact-href \"...\"}"
  ([[line & rest-lines :as lines]
    artifacts-map
    errors
    tests-collected]
   (if line
     (if-let [message (extract-message line)]
       (let [test-name (:source message)
             [rest-message rest-lines]
             (split-with plain-message?
                         rest-lines)
             additional-info
             (cond (and (= :failure (:type message))
                        (stacktrace? rest-message))
                   {:message (:message message)
                    :stacktrace (cstr/join "\n" (cons (:message message)
                                                      rest-message))}
                   (and (tests-collected test-name)
                        (stdout? message))
                   (let [stdout (cstr/join "\n" rest-message)
                         artifact-href (-> stdout
                                           extract-screenshot-name
                                           artifacts-map)]
                     (merge {:stdout stdout}
                            (when artifact-href
                              {:artifact-href artifact-href}))))]
         (if additional-info
           (recur rest-lines
                  artifacts-map
                  (conj errors
                        (merge {:test test-name
                                :display-name (extract-display-name test-name)}
                               additional-info))
                  (conj tests-collected test-name))
           (recur rest-lines artifacts-map errors tests-collected)))
       (recur rest-lines artifacts-map errors tests-collected))
     (->> (group-by :test errors)
          (map (fn [[_ messages]]
                 (apply merge messages))))))
  ([lines artifacts-map]
   (extract-errors
    lines
    artifacts-map
    []
    (hash-set))))
