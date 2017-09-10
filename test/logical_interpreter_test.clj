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
