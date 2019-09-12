(defproject teamcity-error-reporter "0.1.0-SNAPSHOT"
  :description "Extract error-related data from Teamcity selenium tests results"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot teamcity-error-reporter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
