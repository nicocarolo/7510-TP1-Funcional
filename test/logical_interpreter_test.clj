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

(deftest validate-format-input
  (testing "subtract(one, one, two) should be format as [subtract one, one, two]"
    (is (= (format-query "subtract(one, one, two)") ["subtract" "one, one, two"]))
    )
  (testing "subtract(X, Y, Z) :- add(Y, Z, X). should be format as [subtract  X, Y, Z  add  Y, Z, X]"
    (is (= (format-data-line "subtract(X, Y, Z) :- add(Y, Z, X).") ["subtract" "X, Y, Z" "add" "Y, Z, X"]))
    )
  )

(deftest validate-is-fact-test
  (testing "add(one, zero, one) should be fact"
        (is (= (is-fact (format-query "add(one, zero, one)") (set [["add" "one, zero, one"]]))
           true)))
  (testing "add(one, zero, two) should not be fact"
    (let [[database rules] (load-database valid-database)]
        (is (= (is-fact (format-query "add(one, zero, two)") (set [["add" "one, zero, one"]]))
               false))))
  )

(deftest validate-is-rule-test
  (testing "subtract(one, one, two) should be rule"
    (is (= (is-rule [["subtract" "X, Y, Z" "add" "Y, Z, X"]] (format-query "subtract(one, one, two)"))
           true)))
  (testing "subtractts(one, one, two) should not be rule"
    (is (= (is-rule [["subtract" "X, Y, Z" "add" "Y, Z, X"]] (format-query "subtractts(one, one, two)"))
           false)))
  )

(deftest get-params-required-by-rule-test
  (testing "substract(X, Y, Z) should be required X, Y, Z"
      (is (= (get-params-required-by-rule [["subtract" "X, Y, Z" "add" "Y, Z, X"]] (format-query "subtract(one, one, two)"))
             ["X" "Y" "Z"])))
  )

(deftest get-params-received-test
  (testing "subtract(one, one, two) should be received one, one, two"
    (is (= (get-params-received (format-query "subtract(one, one, two)"))
           ["one" "one" "two"])))
  )

(deftest get-facts-to-test-by-rule-test
  (testing "subtract(one, one, two) should be test facts: add Y, Z, X"
    (is (= (get-facts-to-test-by-rule [["subtract" "X, Y, Z" "add" "Y, Z, X"]] (format-query "subtract(one, one, two)"))
           [(seq ["add" "Y, Z, X"])])))

  (testing "hijo(pepe, juan) should be test facts: varon X ; padre Y, X"
    ;(println )
    (is (= (get-facts-to-test-by-rule [["hijo" "X, Y" "varon" "X" "padre" "Y, X"]] (format-query "hijo(pepe, juan)"))
           [(seq ["varon" "X"]) (seq ["padre" "Y, X"])])))
  )