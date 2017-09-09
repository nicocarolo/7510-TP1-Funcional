(ns logical-interpreter
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def number-database "
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

           ;(swap! databaseTwo conj matcher)



     ;(println querys)
     ;(println database)
     [database querys])


(defn loadDatabaseFromString
  [stringData]
  (def database (set nil))
  (def querys (set nil))
  (let [lines (clojure.string/split-lines stringData)]
    (let [filtered (filter #(not (clojure.string/blank? %)) (map clojure.string/trim lines))]
      (def mapped (map #(clojure.string/split % #"\(|\), |\) :- |\.|\)") filtered))
      (doall (map #(if (= (first (str/capitalize (get % 1))) (first (get % 1)))
                     (def querys (conj querys %))
                     (def database (conj database %)))
                   mapped))))


  [database querys])


(defn evaluateRule
  [database replacementsParams factsToTest]
  (println "es Regla")
  (println database)
  (println replacementsParams)
  (println factsToTest)
  )


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
  (let [[database rules] (loadDatabaseFromString database)]
    ;(doall (map println (first querys)))
    (let [parsedQuery (str/split query #"\(|\)|\.|\:-")]
      (println "Evaluando Query.")
      (let [[result replacementsParams factsToTest] (isRule rules parsedQuery)]
        (if (= result true)
          (evaluateRule database replacementsParams factsToTest)
          (if (= (contains? database parsedQuery) true)
            (def result true)
            (def result false)))
        )))
      ;(if (= (isRule querys parsedQuery) true)
      ;  (evaluateRule)
      ;  (if (= (contains? database parsedQuery) true)
      ;    (def result true)
      ;    (def result false)))))
  result)


(defn -main []
    (println (evaluate-query number-database "subtract(one, zero, two)")))
