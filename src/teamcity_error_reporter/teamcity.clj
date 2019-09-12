(ns teamcity-error-reporter.teamcity
  "Interaction with Teamcity server"
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]))


(def ^:dynamic *teamcity-server*
  "http://hati-tcity-srv1.solar.local")


(defn teamcity-request
  "Make a call to TeamCity API, return response via xml/parse"
  [api-method]
  (xml/parse (str *teamcity-server* api-method)))


;; builds


(defn builds
  "Return lazy sequence about all available builds for given id"
  [id]
  (let [payload (teamcity-request (format "/guestAuth/app/rest/buildTypes/id:%s/builds/" id))]
    (->> payload
         :content
         (map (comp teamcity-request :href :attrs)))))


(defn get-build
  "Get single build for given buildType id.
  Take two optional arguments: either :number or :id (as strings)
  (If latter is specified, the former is ignored).
  If no optional arguments specified, returns last build"
  [build-type-id & {:keys [number id]}]
  (let [find-by (fn [k v]
                  (->> (builds build-type-id)
                       (filter (fn [b] (= v (-> b :attrs k))))
                       first))]
    (cond id (find-by :id id)
          number (find-by :number number)
          :else (first (builds build-type-id)))))


;; artifacts


(defn parse-artifact
  "Parse given artifact xml node to more convinient structure.
  Recursively get all child artifacts"
  [{{:keys [href name]} :attrs
    [content] :content}]
  (merge {:name name}
         (if (= :children (:tag content))
           {:children (->> content
                           :attrs
                           :href
                           teamcity-request
                           :content
                           (mapv parse-artifact))}
           {:href (str *teamcity-server* (-> content :attrs :href))})))


(defn build-artifacts
  "Get all artifacts for given build (obtained by teamcity-builds)"
  [build]
  (->> build
       :content
       (filter (comp #{:artifacts} :tag))
       first
       :attrs
       :href
       teamcity-request
       :content
       (mapv parse-artifact)))


(defn artifacts-map
  "Convert list of artifacts into map filename-href.
  Assume all artifact names is unique"
  [artifacts]
  (letfn [(flatten-artifact
            [{:keys [children href name]
              :as artifact}]
            (if children
              (mapcat flatten-artifact
                      children)
              [[name href]]))]
    (into {}
          (mapcat flatten-artifact
                  artifacts))))


;; build log


(defn build-log-url
  "Get url for given build's log"
  [{{:keys [id]} :attrs}]
  (format "%s/guestAuth/downloadBuildLog.html?buildId=%s"
          *teamcity-server* id))
