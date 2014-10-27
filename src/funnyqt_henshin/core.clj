(ns funnyqt-henshin.core
  (:require [clojure.java.io  :as io]
            [clojure.set      :as set]
            [funnyqt.generic  :as g]
            [funnyqt.pmatch   :as pm]
            [funnyqt.query    :as q]
            [funnyqt.in-place :as ip]
            [funnyqt.emf      :as emf]
            [funnyqt.visualization :as viz]
            [funnyqt.polyfns  :as poly]
            [funnyqt.utils    :as u])
  (:import (org.eclipse.emf.ecore EReference EAttribute EClass)
           (org.eclipse.emf.henshin.model.resource HenshinResourceSet)
           (org.eclipse.emf.henshin.model Action GraphElement)))

;;# The code

(def ^{:dynamic true
       :doc "A symbol denoting a rule's model parameter."}
  *the-model-sym*)

(def ^{:dynamic true
       :doc "A map from nodes to symbols.  If a node is preserved and thus occurs in
  the LHS and RHS connected with a Mapping, both map to the same symbol."}
  *node-to-sym-map*)

(def ^{:dynamic true
       :doc "The seq of parameters (symbols) of a rule."}
  *rule-param-syms*)

(def ^{:dynamic true
       :doc "The set of attribute symbols that are preserved."}
  *preserved-attr-syms*)

(def ^{:dynamic true
       :doc "True if the surrounding rule does isomorphic matching."}
  *isomorphic-matching*)

;;## Utils

(defn ^HenshinResourceSet henshin-resource-set [base-path]
  (HenshinResourceSet. base-path))

(defn module [^HenshinResourceSet henshin-resource-set ^String module]
  (.getModule henshin-resource-set module))

(defn resource [^HenshinResourceSet henshin-resource-set ^String res]
  (.getResource henshin-resource-set res))

(defn ^:private mapping-image [node]
  (let [rule (emf/econtainer (g/adj node :graph))]
    (first (for [m (concat (emf/eget rule :mappings)
                           #_(emf/eget rule :multiMappings))
                 :when (= (emf/eget m :origin) node)]
             (emf/eget m :image)))))

;;## get-type

(poly/declare-polyfn get-type
                     "Returns the symbol/keyword denoting a Node/Edge/Attribute type."
                     [el])

(poly/defpolyfn get-type Node [el]
  (g/qname (emf/eget el :type)))

(poly/defpolyfn get-type Edge [el]
  (keyword (.getName ^EReference (emf/eget el :type))))

(poly/defpolyfn get-type Attribute [el]
  (keyword (.getName ^EAttribute (emf/eget el :type))))

;;## preserved / created / deleted

(defn ^:private has-action-type? [ge type]
  (when-let [^Action a (emf/eget ge :action)]
    (= (.getType a) type)))

(defn ^:private preserved? [ge]
  (has-action-type? ge org.eclipse.emf.henshin.model.Action$Type/PRESERVE))

(defn ^:private deleted? [ge]
  (has-action-type? ge org.eclipse.emf.henshin.model.Action$Type/DELETE))

(defn ^:private forbidden? [ge]
  (has-action-type? ge org.eclipse.emf.henshin.model.Action$Type/FORBID))

(defn ^:private created? [ge]
  (has-action-type? ge org.eclipse.emf.henshin.model.Action$Type/CREATE))

(defn ^:private required? [ge]
  (= has-action-type? org.eclipse.emf.henshin.model.Action$Type/REQUIRE))


(defn preserved-ges
  "Returns the set of LHS graph elements that are preserved."
  [lhs-graph]
  (filter preserved? (concat (g/adjs lhs-graph :nodes)
                             (g/adjs lhs-graph :edges))))

(defn deleted-ges
  "Returns the set of LHS graph elements that are deleted."
  [lhs-graph]
  (filter deleted? (concat (g/adjs lhs-graph :nodes)
                           (g/adjs lhs-graph :edges))))

(defn required-ges
  "Returns the set of LHS graph elements that are required."
  [lhs-graph]
  (filter required? (concat (g/adjs lhs-graph :nodes)
                            (g/adjs lhs-graph :edges))))

(defn forbidden-ges
  "Returns the set of LHS graph elements that are forbidden."
  [lhs-graph]
  (filter forbidden? (concat (g/adjs lhs-graph :nodes)
                             (g/adjs lhs-graph :edges))))

(defn created-ges
  "Returns the set of RHS graph elements that are created."
  [rhs-graph]
  (filter created? (concat (g/adjs rhs-graph :nodes)
                           (g/adjs rhs-graph :edges))))

;;## to-pattern-symbol

(poly/declare-polyfn to-pattern-symbol [ge with-type])

(poly/defpolyfn to-pattern-symbol Node [n with-type]
  (symbol (str (*node-to-sym-map* n)
               (when with-type
                 (str "<" (g/qname (emf/eget n :type)) ">")))))

(poly/defpolyfn to-pattern-symbol Edge [n with-type]
  (symbol (str "-<" (keyword (emf/eget (emf/eget n :type) :name)) ">->")))

;;## transform-formula

(poly/declare-polyfn transform-formula [f outer-node-ids])

(defn transform-unary-formula [op f outer-node-ids]
  `(~op ~(transform-formula (emf/eget f :child) outer-node-ids)))

(poly/defpolyfn transform-formula Not [f outer-node-ids]
  (transform-unary-formula `not f outer-node-ids))

(defn transform-binary-formula [op f outer-node-ids]
  `(~op ~(transform-formula (emf/eget f :left) outer-node-ids)
        ~(transform-formula (emf/eget f :right) outer-node-ids)))

(poly/defpolyfn transform-formula And [f outer-node-ids]
  (transform-binary-formula `and f outer-node-ids))

(poly/defpolyfn transform-formula Or [f outer-node-ids]
  (transform-binary-formula `or f outer-node-ids))

(poly/defpolyfn transform-formula Xor [f outer-node-ids]
  (transform-binary-formula `q/xor f outer-node-ids))

(declare graph-to-pattern-vec)
(poly/defpolyfn transform-formula NestedCondition [f outer-node-ids]
  (let [param-ids (set/intersection outer-node-ids
                                    (set (map *node-to-sym-map* (g/adjs f :conclusion :nodes))))]
    `(seq ((pm/pattern ~(gensym "nested-condition")
                       {:pattern-expansion-context :emf}
                       [~*the-model-sym* ~@param-ids]
                       ~(graph-to-pattern-vec (emf/eget f :conclusion)))
           ~*the-model-sym* ~@param-ids))))

;;## transform-unit

(defn ^:private node-id [n]
  (gensym (clojure.string/lower-case (.getName ^EClass (emf/eget n :type)))))

(defn ^:private node-to-id-map [rule]
  (let [base-map (apply hash-map (mapcat (fn [n] [n (node-id n)])
                                         (emf/eallcontents rule 'Node)))
        ms (q/p-apply rule [q/p-seq [q/p-* q/<>--] [q/p-alt :mappings
                                                    :multiMappings]])]
    (merge base-map
           (apply hash-map
                  (mapcat (fn [m]
                            [(emf/eget m :image) (base-map (emf/eget m :origin))])
                          ms)))))

(defn +-or-str [a b]
  (if (or (string? a) (string? b))
    (str a b)
    (+ a b)))

(defn ^:private expand-attribute-value [^String val]
  (if (and (.startsWith val "(")
           (.endsWith val ")"))
    ;; We assume that "(foo bar baz)" is a Clojure form, so keep it that way.
    (read-string val)
    (let [to-clj-fn (fn convert-op [op]
                      (condp = op
                        "+" `+-or-str
                        "-" `-
                        "*" `*
                        "/" `/))
          to-clj-fn-call (fn do-it [v]
                           (loop [v v]
                             (if (> (count v) 1)
                               (let [[a1 a2 op & more] v]
                                 (recur (conj more (list op a2 a1))))
                               (first v))))
          m (re-matcher #"(\w+)\s*([-+*/])?\s*" val)
          r (loop [x (re-find m), r ()]
              (if x
                (let [[_ operand op] x]
                  (recur (re-find m) (conj (if op
                                             (conj r (to-clj-fn op))
                                             r)
                                           (symbol operand))))
                r))]
      (to-clj-fn-call r))))

(defn ^:private handle-lhs-attributes [n]
  ;; If the Attribute's value slot doesn't corresponds to a parameter name, the
  ;; current value must be bound to a symbol which is the value of the
  ;; Attribute.
  ;;
  ;; Else, the host graph element attribute value must match what's in the
  ;; Henshin Attribute's value slot.  (That also applies to forbidden
  ;; attributes because those are in a negative pattern anyway.)
  (mapcat (fn handle-attribute [a]
            (let [value (emf/eget a :value)]
              (if (q/member? (symbol value) *rule-param-syms*)
                `[:when (= ~(expand-attribute-value value)
                           (emf/eget ~(*node-to-sym-map* n) ~(get-type a)))]
                `[:let [~(symbol value) (emf/eget ~(*node-to-sym-map* n) ~(get-type a))]])))
          (emf/eget n :attributes)))

(defn ^:private sort-nodes [nodes]
  (q/sort-topologically
   (fn [n]
     (disj (q/p-apply n [q/p-+ [q/p-seq :incoming :source]]) n))
   nodes))

(defn ^:private graph-to-pattern-vec [graph]
  (let [this-graphs-node-syms (set (map *node-to-sym-map* (emf/eget graph :nodes)))
        multi-mapped-nodes (q/p-apply graph [q/p-seq q/--<>
                                             :multiMappings :image])]
    (into []
          (concat (mapcat (fn [n]
                            (into [(to-pattern-symbol n true)]
                                  (handle-lhs-attributes n)))
                          ;; Sort them so that the matching starts at a vertex
                          ;; that references others.
                          (sort-nodes
                           ;; Consider only nodes that aren't multi-mapped and
                           ;; those which point to non-multi-mapped nodes (to
                           ;; make the pattern anchored a an argument vertex).
                           (mapcat
                            #(conj (g/adjs % :incoming :source) %)
                            (set/difference
                             (set (emf/eget graph :nodes))
                             multi-mapped-nodes))))
                  (mapcat
                   (fn [e]
                     [(to-pattern-symbol (emf/eget e :source) false)
                      (to-pattern-symbol e :ignore)
                      (to-pattern-symbol (emf/eget e :target) false)])
                   ;; Don't consider edges that run between two multi-mapped
                   ;; nodes.
                   (remove (fn [e]
                             (and (multi-mapped-nodes (emf/eget e :source))
                                  (multi-mapped-nodes (emf/eget e :target))))
                           (emf/eget graph :edges)))
                  (when-let [f (emf/eget graph :formula)]
                    [:when (transform-formula f this-graphs-node-syms)])
                  (when *isomorphic-matching*
                    [:isomorphic])))))

(declare transform-rule)
(defn ^:private generate-rule-body [rule]
  (let [pges (preserved-ges (emf/eget rule :lhs))
        dges (deleted-ges (emf/eget rule :lhs))
        cges (created-ges (emf/eget rule :rhs))
        pnodes (set (filter #(g/has-type? % 'Node) pges))
        pedges (set (filter #(g/has-type? % 'Edge) pges))
        dnodes (set (filter #(g/has-type? % 'Node) dges))
        dedges (set (filter #(g/has-type? % 'Edge) dges))
        cnodes (set (filter #(g/has-type? % 'Node) cges))
        cedges (set (filter #(g/has-type? % 'Edge) cges))]
    ;; Create nodes and edges and set attributes
    `(let [~@(doall
              (mapcat (fn [cn]
                        `[~(*node-to-sym-map* cn) (emf/ecreate! ~*the-model-sym* '~(get-type cn))])
                      cnodes))]
       ~@(doall
          (mapcat
           (fn [[node-sym atts]]
             (for [a atts
                   :when (not (*preserved-attr-syms* (symbol (emf/eget a :value))))]
               `(emf/eset! ~node-sym ~(get-type a) ~(expand-attribute-value
                                                     (emf/eget a :value)))))
           (map (fn [n]
                  [(*node-to-sym-map* n) (emf/eget n :attributes)])
                (concat cnodes (map mapping-image pnodes)))))
       ~@(doall
          (for [ce cedges]
            `(emf/eadd! ~(*node-to-sym-map* (emf/eget ce :source))
                        ~(get-type ce)
                        ~(*node-to-sym-map* (emf/eget ce :target)))))
       ;; Handle multi-rules
       ~@(doall
          (for [mrule (emf/eget rule :multiRules)]
            (binding [*rule-param-syms* (map *node-to-sym-map*
                                             (g/adjs mrule :mappings :origin))]
              `(~(transform-rule mrule (gensym "multirule") true)
                ~*the-model-sym* ~@*rule-param-syms*))))
       ;; Delete nodes and edges
       ~@(doall
          (for [dn dnodes]
            `(emf/edelete! ~(*node-to-sym-map* dn))))
       ~@(doall
          (for [de dedges
                :when (not (or (dnodes (emf/eget de :source))
                               (dnodes (emf/eget de :target))))
                :let [s (*node-to-sym-map* (emf/eget de :source))
                      t (*node-to-sym-map* (emf/eget de :target))]]
            `(emf/eremove! ~s ~(get-type de) ~t))))))

(defn ^:private transform-rule [rule name is-multi]
  (binding [*isomorphic-matching* (emf/eget rule :injectiveMatching)]
    `(ip/rule ~name
              ~(let [opts {:pattern-expansion-context :emf}]
                 (if is-multi
                   (assoc opts :forall true)
                   opts))
              [~*the-model-sym* ~@*rule-param-syms*]
              ~(graph-to-pattern-vec (emf/eget rule :lhs))
              ~(generate-rule-body rule))))

(poly/declare-polyfn transform-unit
                     "Returns a vector [name val] that can be interned in some ns.
  `name` is a symbol (possibly with metadata), val is a function/pattern/rule."
                     [unit]
                     (println "Don't yet know how to handle unit" unit))

(poly/defpolyfn transform-unit Rule [rule]
  (binding [*node-to-sym-map* (node-to-id-map rule)
            *the-model-sym* (gensym "model")
            *rule-param-syms* (map #(symbol (emf/eget % :name))
                                   (emf/eget rule :parameters))
            *preserved-attr-syms* (set (map #(symbol (emf/eget % :value))
                                            (q/p-apply rule
                                                       [q/p-seq
                                                        :lhs
                                                        [q/p-* q/<>--]
                                                        [q/p-restr 'Attribute preserved?]])))]
    (let [name (emf/eget rule :name)
          name (if (seq name)
                 (symbol name)
                 (u/errorf "Rule without name: %s" rule))
          name (with-meta name {:arglists `([~*the-model-sym* ~@*rule-param-syms*])})]
      [name (transform-rule rule name false)])))

;;## Visualization for debugging

(defn ^:private viz-includes [hm hrs]
  (conj (set (concat (emf/eallcontents hm)
                     (filter
                      (fn [el]
                        (q/exists?
                         #(= el (emf/eget % :type))
                         (emf/eallcontents hm '[Node Edge Attribute])))
                      (emf/eallcontents hrs))))
        hm))

(defn ^:private viz-includes-rule [hm rule hrs]
  (let [r (q/the #(= rule (emf/eget % :name))
                 (emf/eallcontents hm 'Rule))]
    (conj (set (concat (emf/eallcontents r)
                       (filter
                        (fn [el]
                          (q/exists?
                           #(= el (emf/eget % :type))
                           (emf/eallcontents hm '[Node Edge Attribute])))
                        (emf/eallcontents hrs))))
          r)))

;;## Main

(defmacro henshin-to-funnyqt [base-path henshin-model target-ns-sym alias]
  (let [hrs (henshin-resource-set base-path)
        hm (module hrs henshin-model)
        mm (resource hrs "bank.ecore")
        hr (resource hrs "bank.henshin")
        top-units (emf/eget hm :rules)]
    #_(viz/print-model hrs "/home/horn/bank.pdf" :include (viz-includes hm hrs))
    #_(viz/print-model hrs "/home/horn/bank-createAccount.pdf"
                       :include (viz-includes-rule hm "createAccount" hrs))
    #_(viz/print-model hrs "/home/horn/bank-deleteAllAccounts.pdf"
                       :include (viz-includes-rule hm "deleteAllAccounts" hrs))
    #_(viz/print-model hrs "/home/horn/bank-multi-rule.pdf"
                       :include (viz-includes-rule hm "" hrs))
    `(do
       (create-ns '~target-ns-sym)
       ~@(for [unit top-units
               :let [[name definition] (transform-unit unit)]]
           `(intern '~target-ns-sym '~name ~definition))
       ~(when alias
          `(alias '~alias '~target-ns-sym)))))
