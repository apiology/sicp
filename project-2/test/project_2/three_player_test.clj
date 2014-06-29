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


(deftest get-probability-of-c-test-1
  (testing
      (let [summary 
            (make-history-summary
             '("c" "c" "c" "c")
             '("d" "d" "d" "c")
             '("d" "d" "c" "c"))]
        (is (= '(1 1 1)
               (get-probability-of-c summary))))))

(deftest get-probability-of-c-test-2
  (testing
      (let [summary 
            (make-history-summary
             '("c" "c" "c" "d" "c")
             '("d" "c" "d" "d" "c")
             '("d" "c" "c" "c" "c"))]
        (is (= '(1/2 1 nil)
               (get-probability-of-c summary))))))

(deftest test-entry-test-1
  (testing (is (= true
                  (test-entry '(5 9 23) '(5 9 23))))
    (is (= false
           (test-entry '(5 9 23) '(5 9 23 9))))
    (is (= false
           (test-entry '(5 9 23) '(5 9 22))))
    (is (= false
           (test-entry '(5 9 23) '(5 9 nil))))))

(def patsy-history
  '(("c" "c" "c" "c" "c" "c")
    ("d" "d" "c" "d" "d" "c")
    ("d" "d" "c" "c" "c" "c")))

(def possible-patsy-history
  '(("c" "c" "c" "c")
    ("c" "d" "d" "c")
    ("c" "c" "c" "c")))


(def nasty-history
  '(("d" "d" "d" "d" "d")
    ("d" "c" "d" "d" "c")
    ("d" "c" "c" "c" "c")))

(def soft-eye-for-eye-history
  '(("d" "c" "c" "c" "c" "c")
    ("d" "d" "c" "d" "d" "c")
    ("d" "d" "c" "c" "c" "c")))

(deftest is-he-a-fool?-test-1
  (testing (is (= true (apply is-he-a-fool? patsy-history)))
    (is (= false (apply is-he-a-fool? nasty-history)))
    (is (= false (apply is-he-a-fool? possible-patsy-history)))))

(deftest could-he-be-a-fool?-test-1
  (testing (is (= true (apply could-he-be-a-fool? patsy-history)))
    (is (= false (apply could-he-be-a-fool? nasty-history)))
    (is (= true (apply could-he-be-a-fool? possible-patsy-history)))))

(deftest is-he-soft-eye-for-eye?-test-1
  (testing (is (= true (apply is-he-soft-eye-for-eye? soft-eye-for-eye-history)))
    (is (= false (apply is-he-soft-eye-for-eye? patsy-history)))
    (is (= false (apply is-he-soft-eye-for-eye? nasty-history)))))

         
(get-probability-of-c (apply make-history-summary soft-eye-for-eye-history))

(get-probability-of-c (apply make-history-summary possible-patsy-history))
(test-entry (list 1 1 1) (list 1 1 nil))

