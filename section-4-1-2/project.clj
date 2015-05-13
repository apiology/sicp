(defproject section-4-1-2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.typed "0.2.84"]
                 [slamhound "1.5.5"]]
  :plugins [[lein-typed "0.3.5"]
            [lein-cloverage "1.0.2"]
            [lein-kibit "0.1.2"]
            [lein-ancient "0.6.7"]
            [jonase/eastwood "0.2.1"]]
  :core.typed {:check [section-4-1-2.environment section-4-1-2.util]}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]})




