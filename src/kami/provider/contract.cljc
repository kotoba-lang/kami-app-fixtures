(ns kami.provider.contract
  "Local provider split contract validation."
  (:require #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.string :as str])))

(def contract-resource "kami/provider/split_contract.edn")
(def expected-repo "orgs/kotoba-lang/kami-app-fixtures")

#?(:clj
   (defn contract []
     (let [resource (io/resource contract-resource)]
       (when-not resource
         (throw (ex-info "missing provider split contract" {:resource contract-resource})))
       (-> resource slurp edn/read-string))))

(def required-keys
  #{:kami/split-to :kami/kind :kami/authority :kami/adapter :kami/families
    :kami/worlds :kami/provider-namespaces :kami/legacy-crates
    :kami/authority-repos :kami/exit-criteria})

(def kinds
  #{:wasm-component-provider :component-runtime-provider :edn-fixture-provider})

(def required-legacy-crates
  #{"kami-clj-play" "kami-clj-play3d"})

(def required-exit-criteria
  #{:convert-to-edn-fixtures-or-delete})

(defn- err [path message]
  {:path path :message message})

(defn- collect-errors [& xs]
  (vec (remove nil? (mapcat #(if (sequential? %) % [%]) xs))))

(defn- non-empty-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- non-empty-string-vector? [x]
  (and (vector? x) (seq x) (every? non-empty-string? x)))

(defn- non-empty-keyword-vector? [x]
  (and (vector? x) (seq x) (every? keyword? x)))

(defn- missing-errors [m]
  (mapv #(err [%] "required key is missing")
        (sort (remove #(contains? m %) required-keys))))

(defn- field-error [m k pred message]
  (when (and (contains? m k) (not (pred (get m k))))
    (err [k] message)))

(defn validate-contract [m]
  (let [errors
        (if-not (map? m)
          [(err [] "provider split contract must be a map")]
          (collect-errors
           (missing-errors m)
           (field-error m :kami/split-to #{expected-repo}
                        ":kami/split-to must match this repo")
           (field-error m :kami/kind kinds
                        ":kami/kind must be a provider repo kind")
           (field-error m :kami/authority #{[:kotoba-clj :edn]}
                        ":kami/authority must be [:kotoba-clj :edn]")
           (field-error m :kami/adapter #{:wasm-component-model}
                        ":kami/adapter must be :wasm-component-model")
           (field-error m :kami/families non-empty-keyword-vector?
                        ":kami/families must be a non-empty vector of keywords")
           (field-error m :kami/worlds non-empty-keyword-vector?
                        ":kami/worlds must be a non-empty vector of keywords")
           (field-error m :kami/provider-namespaces non-empty-string-vector?
                        ":kami/provider-namespaces must be a non-empty vector of strings")
           (field-error m :kami/legacy-crates non-empty-string-vector?
                        ":kami/legacy-crates must be a non-empty vector of strings")
           (field-error m :kami/authority-repos non-empty-string-vector?
                        ":kami/authority-repos must be a non-empty vector of strings")
           (field-error m :kami/exit-criteria non-empty-keyword-vector?
                        ":kami/exit-criteria must be a non-empty vector of keywords")
           (when-not (= :edn-fixture-provider (:kami/kind m))
             (err [:kami/kind] ":kami/kind must be :edn-fixture-provider for app fixtures"))
           (when-not (= [:app-fixtures] (:kami/families m))
             (err [:kami/families] ":kami/families must be [:app-fixtures]"))
           (when-not (= [:kami/scene] (:kami/worlds m))
             (err [:kami/worlds] ":kami/worlds must be [:kami/scene]"))
           (when-not (= ["kami.providers.app_fixtures"] (:kami/provider-namespaces m))
             (err [:kami/provider-namespaces] ":kami/provider-namespaces must name the app fixture provider namespace"))
           (when-not (every? (set (:kami/legacy-crates m)) required-legacy-crates)
             (err [:kami/legacy-crates] "legacy CLJ player crates must be tracked as fixture migration sources"))
           (when-not (= required-exit-criteria (set (:kami/exit-criteria m)))
             (err [:kami/exit-criteria] "exit criteria must require conversion to EDN fixtures or deletion"))))]
    {:valid? (empty? errors) :errors errors}))

(defn contract? [m]
  (:valid? (validate-contract m)))
