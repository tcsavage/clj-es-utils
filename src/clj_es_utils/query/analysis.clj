(ns clj-es-utils.query.analysis
  "Static analysis on queries."
  (:require [clj-es-utils.query.core :as query]))

(defn query-node-counts
  "Count the frequencies of different node types.
  Returns a map of {TAG FREQUENCY}."
  [query]
  (let [counter (atom (zipmap query/*node-tags* (repeat 0)))]
    (letfn [(walker [node]
              (swap! counter update (query/node-tag node) inc)
              node)]
      (query/walk-query walker query)
      @counter)))

(defn- summarise-counter
  [counter]
  (letfn [(rf [summary [_ counter]]
              (cond-> summary
                :always (update :total inc)
                :always (update :real-total + counter)
                (= 1 counter) (update :unique inc)
                (< 1 counter) (update :duplicated inc)))]
         (reduce rf {:total 0 :duplicated 0 :unique 0 :real-total 0} counter)))

(defn match-term-id-counts
  "Counts-up the different values used in match/term/id nodes and returns
  summaries containing various totals:
  * :total - the number of nodes containing that value
  * :real-total - the total number of occurrences in all nodes
  * :unique - the number of distinct values
  * :duplicated - the number of values which are duplicated across many nodes
  Ideally :duplicated would be 0 and the remaining fields all the same."
  [query]
  (let [match-counter (atom {})
        term-counter (atom {})
        id-counter (atom {})
        ignored-fields #{:boost :adjust_pure_negative :minimum_should_match}
        extract-value (some-fn :value :values :query)]
    (letfn [(singleton [x] {x 1})
            (merge-add-atom [a m] (swap! a (partial merge-with +) m))
            (query-value [data] (if (map? data) (extract-value data) data))
            (select-query [data] (->> data (remove (comp ignored-fields key)) first val))
            (walker [node]
                    (case (query/node-tag node)
                      :multi_match (->> node :multi_match :query singleton (merge-add-atom match-counter))
                      :match (->> node :match select-query query-value singleton (merge-add-atom match-counter))
                      :match_phrase (->> node :match_phrase select-query query-value singleton (merge-add-atom match-counter))
                      :terms (->> node :terms select-query query-value frequencies (merge-add-atom term-counter))
                      :term (->> node :term select-query query-value singleton (merge-add-atom term-counter))
                      :ids (->> node :ids query-value frequencies (merge-add-atom id-counter))
                      nil)
                    node)]
      (query/walk-query walker query)
      {:matches (summarise-counter @match-counter)
       :terms (summarise-counter @term-counter)
       :ids (summarise-counter @id-counter)})))

(defn static-query-stats
  "Generate various stats for a query by static analysis."
  [query]
  (let [node-counts (query-node-counts query)]
    {:node-counts node-counts
     :total-nodes (apply + (vals node-counts))
     :counts (match-term-id-counts query)
     :edn-chars (count (pr-str query))}))
