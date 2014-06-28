(ns project-2.two-player-test
  (:require [clojure.test :refer :all]
            [project-2.two-player :refer :all]
            [project-2.core :refer :all]))

(deftest extract-entry-test
  (testing "Extractions work"
    (is (= '(("c" "c") (3 3))
           (extract-entry '("c" "c") game-association-list)))
    (is (= '(("d" "c") (5 0))
           (extract-entry '("d" "c") game-association-list)))))

;; (run-tests)
