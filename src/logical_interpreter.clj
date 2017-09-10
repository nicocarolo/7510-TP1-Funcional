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

(def data-file (io/resource "src/hello.txt"))

(defn loadDatabaseFromFile
     [filePath]
     (with-open [rdr (clojure.java.io/reader filePath)]
       (def database (set nil))
       (def querys (set nil))
       (def databaseTwo (atom {}))
       (println "Cargando Base de Datos.")
       (doseq [line (line-seq rdr)]
         (let [matcher (str/split line #"\(|\)|\.|\ :-")]
           ;(println (str/capitalize (get matcher 1)))
           ;(println (get matcher 1))
           (if (= (first (str/capitalize (get matcher 1))) (first (get matcher 1)))
             (def querys (conj querys matcher))
             (def database (conj database matcher))))))
     [database querys])

(defn validate-input
  [line]
  (if (nil? (re-find (re-pattern "([a-zA-Z]*)(\\((.*\\)))") line))
    false
    true
    )
  )

(defn loadDatabaseFromString
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

(defn isFact
  [fact database]
  (def result false)
  (if (= (contains? database fact) true)
    (def result true)
    (def result false))
  result
  )

(defn replace-several
  [string replacementsParams]
  (def cadena {})
  (def paramsCount (count (get replacementsParams 0)))
  (if (= paramsCount 1)
    (def replaces {(name (get (get replacementsParams 0) 0))
                  (name (get (get replacementsParams 1) 0))
                  })
    (if (= paramsCount 2)
      (def replaces {(name (get (get replacementsParams 0) 0))
                    (name (get (get replacementsParams 1) 0))
                    (name (get (get replacementsParams 0) 1))
                    (name (get (get replacementsParams 1) 1))
                    })
      (if (= paramsCount 3)
        (def replaces {(name (get (get replacementsParams 0) 0))
                      (name (get (get replacementsParams 1) 0))
                      (name (get (get replacementsParams 0) 1))
                      (name (get (get replacementsParams 1) 1))
                      (name (get (get replacementsParams 0) 2))
                      (name (get (get replacementsParams 1) 2))
                      })
        ))
    )
  (def possibleChars (str/join "|" (get replacementsParams 0)))
  (str/replace string (re-pattern possibleChars) replaces)
  )

(defn evaluateRule
  [database replacementsParams factsToTest]
  (def ruleValue true)
  (doall (map #(
                 if (= (isFact [(nth % 0) (replace-several (nth % 1) replacementsParams)] database) false)
                 (def ruleValue false)
                 nil
                 ) factsToTest))
  ruleValue)

(defn isRule
  [rules query]
  (def result false)
  (def replacementsParams (vector))
  (def factsToTest (vector))
  (doall (map #(
                 if (= (first query) (first %))
                   (let [paramsRequired (str/split (get % 1) #"\, ")]
                     (let [paramsReceived (str/split (get query 1) #"\, ")]
                      (if (= (count paramsRequired) (count paramsReceived))
                        [
                         (def replacementsParams [paramsRequired paramsReceived])
                         (def factsToTest (into [] (partition 2 (subvec % 2))))
                         (def result true)
                         ]
                        nil)))
                   (def result false))
           rules))
  [result replacementsParams factsToTest])



(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (if (= (validate-input query) false)
    (def result nil)
    (let [[database rules] (loadDatabaseFromString database)]
      (if (empty? database)
        (def result nil)
        (let [parsedQuery (str/split query #"\(|\)|\.|\:-")]
          (println "Evaluando Query.")
          (let [[result replacementsParams factsToTest] (isRule rules parsedQuery)]
            (if (= result true)
              (if (= (evaluateRule database replacementsParams factsToTest) true)
                (def result true)
                (def result false))
              (if (= (isFact parsedQuery database) true)
                (def result true)
                (def result false))
              ))))))
  (if (nil? result)
    (println "Ocurrio un error con la base de datos o consulta")
    (println (str "La consulta: " query " es: " result)))
  result)


;(defn -main []
;    (println (evaluate-query number-database "subtract(two, one, one)")))