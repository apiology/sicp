(ns project-2.three-player-test
  (:require [clojure.test :refer :all]
            [project-2.three-player :refer :all]
            [project-2.core :refer :all]))

(deftest extract-entry-test
  (testing "Extractions work"
    (is (= '(("c" "c" "c") (4 4 4))
           (extract-entry '("c" "c" "c") game-association-list)))
    (is (= '(("d" "c" "d") (3 0 3))
           (extract-entry '("d" "c" "d") game-association-list)))))

;; (run-tests)

(deftest extract-entry-test
  (testing "Extractions work"
    (is (= '(("c" "c" "c") (4 4 4))
           (extract-entry '("c" "c" "c") game-association-list)))
    (is (= '(("d" "c" "d") (3 0 3))
           (extract-entry '("d" "c" "d") game-association-list)))))


(deftest make-history-summary-test-1
  (testing
    (is (= '((1 0 1) (0 0 0) (0 0 0))
           (make-history-summary
            '("c" "c")
            '("c" "c")
            '("c" "c"))))))

(deftest make-history-summary-test-2
  (testing
    (is (= '((1 0 1) (1 0 1) (0 1 1))
           (make-history-summary
            '("c" "c" "d" "d")
            '("c" "c" "c" "d")
            '("c" "c" "d" "d"))))))


(deftest make-history-summary-test-3
  (testing
    (is (= '((3 0 3) (1 1 2) (0 2 2))
           (make-history-summary
            '("c" "c" "d" "d" "c" "d" "c" "c")
            '("c" "c" "c" "d" "d" "c" "d" "c")
            '("c" "c" "d" "d" "d" "c" "c" "c"))))))


