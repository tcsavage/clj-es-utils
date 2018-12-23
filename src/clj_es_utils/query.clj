(ns clj-es-utils.query
  "Utilities for operating on the Elasticsearch query DSL."
  (:require [clojure.walk :as walk]))

(def ^:dynamic *node-tags*
  "The set of tags for which ES query nodes will be detected."
  #{:bool :terms :term :match :multi_match :nested :range :ids})

(defn node-tag
  "Returns the tag of an ES query node if it's a valid node.
  Throws if the input is not a valid node structure, or the tag is unknown."
  [node]
  (if (vector? node)
    (node-tag (into {} [node]))
    (let [ks (keys node)]
      (when (not= 1 (count ks))
        (throw (ex-info "Invalid node structure" {:node node})))
      (when (not (contains? *node-tags* (first ks)))
        (throw (ex-info "Not a known Elasticsearch node type" {:tag (first ks)})))
      (first ks))))

(defn node?
  "Tests whether some data is an ES query node."
  [data]
  (and
    (some? data)
    (map? data)
    (= 1 (count (keys data)))
    (contains? *node-tags* (first (keys data)))))

(defn walk-query
  "Performs a depth-first, post-order traversal of the given query, applying
  alg to every node encountered.
  alg must be a function which takes a node with any children replaced by
  whatever alg returns."
  [alg query]
  (walk/postwalk #(if (node? %) (alg %) %) query))

