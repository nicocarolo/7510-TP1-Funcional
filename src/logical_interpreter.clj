(ns logical-interpreter
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

;(def number-database "
;    add(zero, zero, zero).
;    add(zero, one, one).
;    add(zero, two, two).
;    add(one, zero, one).
;    add(one, one, two).
;    add(one, two, zero).
;    add(two, zero, two).
;    add(two, one, zero).
;    add(two, two, one).
;    subtract(X, Y, Z) :- add(Y, Z, X).
;  ")

(defn validate-input
  "Validate string input by the following regular expression: .([a-zA-Z]*)(\\((.*\\)))
  Must match a word follow by '(' and ')' chars.
  Return true if pattern match, false if not."
  [line]
  (if (nil? (re-find (re-pattern ".([a-zA-Z]*)(\\((.*\\)))") line))
    false
    true
    )
  )

(defn load-database
  "Store string input in set of vectors.
  Facts are splitted in [predicate     value1, value2, ...., valueN].
  Rules are splitted in [predicate     value1, value2, ...., valueN
                        predicateFact1 value1, value2, ...., valueN
                        ...........................................
                        predicateFactN value1, value2, ...., valueN].
  If params in line are capitalize then the splitted line
  will save as rule, else the splitted line will save as fact on database."
  [stringData]
  (def database (set nil))
  (def rules (set nil))
  (let [lines (str/split-lines stringData)]
    (let [filtered (filter #(not (str/blank? %)) (map str/trim lines))]
      (if (not-every? #(validate-input %) filtered)
        nil
        [(def mapped (map #(str/split % #"\(|\), |\) :- |\.|\)") filtered))
         (if (nil? mapped)
           nil
           (doall (map #(if (= (first (str/capitalize (get % 1))) (first (get % 1)))
                          (def rules (conj rules %))
                          (def database (conj database %)))
                       mapped))
           )]
      )))
  [database rules])

(defn is-fact
  "Check if database contains splitted fact.
  Return true if contain, false if not."
  [fact database]
  (def result false)
  (if (= (contains? database fact) true)
    (def result true)
    (def result false))
  result
  )

(defn replace-several
  "Return string received with values replaced by replacementsParams criteria.
  Example:
    string = Y, X
    replacementsParams = [[X Y] [pepe juan]]
    return 'juan, pepe'"
  [string replacementsParams]
  (def possibleChars (str/join "|" (get replacementsParams 0)))
  (str/replace
    string
    (re-pattern possibleChars)
    (apply hash-map (interleave (get replacementsParams 0) (get replacementsParams 1)))
    )
  )

(defn evaluate-rule
  "Evaluate rule checking if every on factsToTest is fact."
  [database replacementsParams factsToTest]
  (= (every? #(is-fact [(nth % 0) (replace-several (nth % 1) replacementsParams)] database) factsToTest)
    true)
  )

(defn is-rule
  "Return true if splitted query is contain in rules,
  false if not."
  [rules query]
  (if (nil? (some #(= (first query) (first %)) rules))
    false
    true
    )
  )

(defn get-params-required-by-rule
  "Return vector of params required by the splitted query received,
  getting the rule format on rules."
  [rules query]
  (str/split
    (get
      (nth
        (filter
          #(= (first query) (first %))
          rules)
        0)
      1)
  #"\, ")
  )

(defn get-params-received
  "Return vector of params received on the splitted query received."
  [query]
  (str/split (get query 1) #"\, ")
  )

(defn get-facts-to-test-by-rule
  "Return vector of facts to test required by the splitted query received,
  getting the rule format on rules."
  [rules query]
  (into []
    (partition 2
      (subvec
        (nth
          (filter
            #(= (first query) (first %))
            rules)
          0)
        2)
      )
  )
  )


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (if (= (validate-input query) false)
    (def result nil)
    (let [[database rules] (load-database database)]
      (if (empty? database)
        (def result nil)
        (let [parsedQuery (str/split query #"\(|\)|\.|\:-")]
          ;(println "Evaluando Query.")
          (if (= (is-rule rules parsedQuery) true)
            (if (= (evaluate-rule database
                                  [(get-params-required-by-rule rules parsedQuery)
                                  (get-params-received parsedQuery)]
                                  (get-facts-to-test-by-rule rules parsedQuery)) true)
              (def result true)
              (def result false))
            (if (= (is-fact parsedQuery database) true)
              (def result true)
              (def result false))
            )))))
  result)


;(defn -main []
;    (println (evaluate-query number-database "subtract(two, one, one)")))