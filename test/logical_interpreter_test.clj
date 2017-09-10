(ns logical-interpreter-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def valid-database "
	add(zero, zero, zero).
	add(zero, one, one).
	add(zero, two, two).
	add(one, zero, one).
	add(one, one, two).
	add(one, two, zero).
	add(two, zero, two).
	add(two, one, zero).
	add(two, two, one).
	subtract(X, Y, Z) :- add(Y, Z, X).
")
(def invalid-fact-database "
	add(zero, zero, zero).
	add(.
	subtract(X, Y, Z) :- add(Y, Z, X).
")
(def invalid-rule-database "
	add(zero, zero, zero).
	subtract(.
")

(deftest incomplete-database-test
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query invalid-fact-database "varon(juan)")
           nil)))
  (testing "varon(maria) should be nil"
    (is (= (evaluate-query invalid-rule-database "varon(maria)")
           nil)))
  )

(deftest incomplete-query-test
  (testing "varon( should be nil"
    (is (= (evaluate-query valid-database "varon(")
           nil)))
  (testing "varon) should be nil"
    (is (= (evaluate-query valid-database "varon)")
           nil)))
  )

(deftest validate-input-test
  (testing "subtract(one, one, two) should be true"
    (is (= (validate-input "subtract(one, one, two)")
           true)))
  (testing "subtract(one) should be true"
    (is (= (validate-input "subtract(one)")
           true)))
  (testing "subtract( should be false"
    (is (= (validate-input "subtract(")
           false)))
  (testing "(one) should be false"
    (is (= (validate-input "(one)")
           false)))
  (testing "subtract) should be false"
    (is (= (validate-input "subtract)")
           false)))
  )

(deftest validate-is-fact-test
  (testing "add(one, zero, one) should be fact"
    (let [[database rules] (load-database valid-database)]
        (is (= (is-fact (clojure.string/split "add(one, zero, one)" #"\(|\)|\.|\:-") database)
           true))))
  (testing "add(one, zero, two) should not be fact"
    (let [[database rules] (load-database valid-database)]
        (is (= (is-fact (clojure.string/split "add(one, zero, two)" #"\(|\)|\.|\:-") database)
               false))))
  )

(deftest validate-is-rule-test
  (testing "subtract(one, one, two) should be rule"
    (let [[database rules] (load-database valid-database)]
      (is (= (is-rule rules (clojure.string/split "subtract(one, one, two)" #"\(|\)|\.|\:-"))
             true))))
  (testing "subtractts(one, one, two) should not be rule"
    (let [[database rules] (load-database valid-database)]
      (is (= (is-rule rules (clojure.string/split "subtractts(one, one, two)" #"\(|\)|\.|\:-"))
             false))))
  )