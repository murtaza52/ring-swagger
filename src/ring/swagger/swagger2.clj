(ns ring.swagger.swagger2
  (:require [clojure.string :as str]
            [schema.core :as s]
            [ring.swagger.common :refer :all]
            [ring.swagger.json-schema :as jsons]
            [ring.swagger.core :as rsc]
            [ring.swagger.swagger2-schema :as schema]))

;; Stuff from prismatic plumbing

(defmacro for-map
  "Like 'for' for building maps. Same bindings except the body should have a
  key-expression and value-expression. If a key is repeated, the last
  value (according to \"for\" semantics) will be retained.

  (= (for-map [i (range 2) j (range 2)] [i j] (even? (+ i j)))
     {[0 0] true, [0 1] false, [1 0] false, [1 1] true})

  An optional symbol can be passed as a first argument, which will be
  bound to the transient map containing the entries produced so far."
  ([seq-exprs key-expr val-expr]
   `(for-map ~(gensym "m") ~seq-exprs ~key-expr ~val-expr))
  ([m-sym seq-exprs key-expr val-expr]
   `(let [m-atom# (atom (transient {}))]
      (doseq ~seq-exprs
        (let [~m-sym @m-atom#]
          (reset! m-atom# (assoc! ~m-sym ~key-expr ~val-expr))))
      (persistent! @m-atom#))))


;;
;; Schema transformations
;;

(defn extract-models [swagger]
  (let [route-meta (->> swagger
                        :paths
                        vals
                        (map vals)
                        flatten)
        body-models (->> route-meta
                         (map (comp :body :parameters)))
        response-models (->> route-meta
                             (map :responses)
                             (mapcat vals)
                             (keep :schema))]
    (concat body-models response-models)))

(defn transform-models [schemas options]
  (->> schemas
       rsc/collect-models
       (rsc/handle-duplicate-schemas (:handle-duplicate-schemas-fn options))
       (map (juxt (comp str key) (comp jsons/schema-object val)))
       (into (sorted-map))))

;;
;; Paths, parameters, responses
;;

(defmulti ^:private extract-parameter (fn [in _ _] in))

(defmethod extract-parameter :body [_ model options]
  (if-let [schema (rsc/peek-schema model)]
    (let [schema-json (jsons/->swagger model options)]
      (vector {:in "body"
               :name (name (s/schema-name schema))
               :description (or (:description (jsons/->swagger schema options)) "")
               :required (not (jsons/maybe? model))
               :schema (dissoc schema-json :description)}))))

(defmethod extract-parameter :default [in model options]
  (if model
    (for [[k v] (-> model value-of rsc/strict-schema)
          :when (s/specific-key? k)
          :let [rk (s/explicit-schema-key k)
                json-schema (jsons/->swagger v options)]
          :when json-schema]
      (merge
        {:in (name in)
         :name (name rk)
         :description ""
         :required (or (= in :path) (s/required-key? k))}
        json-schema))))

(defn- default-response-description
  "uses option :default-response-description-fn to generate
   a default response description for status code"
  [status options]
  (if-let [generator (:default-response-description-fn options)]
    (generator status)
    ""))

(defn convert-parameters [parameters options]
  (into [] (mapcat (fn [[in model]]
                     (extract-parameter in model (assoc options :in in)))
                   parameters)))

(defn convert-responses [responses options]
  (let [responses (for-map [[k v] responses
                            :let [{:keys [schema headers]} v]]
                    k (-> v
                          (cond-> schema (update-in [:schema] jsons/->swagger options))
                          (cond-> headers (update-in [:headers] jsons/properties))
                          (update-in [:description] #(or % (default-response-description k options)))
                          remove-empty-keys))]
    (if-not (empty? responses)
      responses
      {:default {:description ""}})))

(defn convert-operation
  "Returns a map with methods as keys and the Operation
   maps with parameters and responses transformed to comply
   with Swagger spec as values"
  [operation options]
  (for-map [[k v] operation]
    k (-> v
          (update-in-or-remove-key [:parameters] #(convert-parameters % options) empty?)
          (update-in [:responses] convert-responses options))))

(defn swagger-path
  "Replaces Compojure/Clout style path params in uri with Swagger style
  path params.

  Does not support wildcard-paths or inline-regexes.

  The regex is copied from Clout."
  [uri]
  ;; TODO: In 1.0, leave it to client libs to build swagger style path template
  ;; Currently everyone needs to build Clout path is just extra step for all but
  ;; compojure-api.
  (str/replace uri #":([\p{L}_][\p{L}_0-9-]*)" "{$1}"))

(defn extract-paths-and-definitions [swagger options]
  (let [original-paths (or (:paths swagger) {})
        paths (reduce-kv
                (fn [acc k v]
                  (assoc acc
                    (swagger-path k)
                    (convert-operation v options)))
                (empty original-paths)
                original-paths)
        definitions (-> swagger
                        extract-models
                        (transform-models options))]
    [paths definitions]))

(defn ensure-body-sub-schemas [route]
  (if (get-in route [:parameters :body])
    (update-in route [:parameters :body] #(rsc/with-named-sub-schemas % "Body"))
    route))

(defn ensure-response-sub-schemas [route]
  (if-let [responses (get-in route [:responses])]
    (let [schema-codes (reduce (fn [acc [k {:keys [schema]}]]
                                 (if schema (conj acc k) acc))
                               [] responses)
          transformed (reduce (fn [acc code]
                                (update-in acc [:responses code :schema] #(rsc/with-named-sub-schemas % "Response")))
                              route schema-codes)]
      transformed)
    route))

;;
;; Public API
;;

;;
;; Transforming the spec
;;

(defn transform-operations
  "Transforms the operations under the :paths of a ring-swagger spec by applying (f operation)
  to all operations. If the function returns nil, the given operation is removed."
  [f swagger]
  (let [initial-paths (:paths swagger)
        transformed (for [[path endpoints] initial-paths
                          [method endpoint] endpoints
                          :let [endpoint (f endpoint)]]
                      [[path method] endpoint])
        paths (reduce (fn [acc [kv endpoint]]
                        (if endpoint
                          (assoc-in acc kv endpoint)
                          acc)) (empty initial-paths) transformed)]
    (assoc-in swagger [:paths] paths)))

(defn ensure-body-and-response-schema-names
  "Takes a ring-swagger spec and returns a new version
   with a generated names for all anonymous nested schemas
   that come as body parameters or response models."
  [swagger]
  (->> swagger
       (transform-operations ensure-body-sub-schemas)
       (transform-operations ensure-response-sub-schemas)))

;;
;; Schema
;;

(def swagger-defaults {:swagger "2.0"
                       :info {:title "Swagger API"
                              :version "0.0.1"}
                       :produces ["application/json"]
                       :consumes ["application/json"]})

;;
;; Swagger Spec
;;

(def Swagger schema/Swagger)

(def Options {(s/optional-key :ignore-missing-mappings?) s/Bool
              (s/optional-key :default-response-description-fn) (s/=> s/Str s/Int)
              (s/optional-key :handle-duplicate-schemas-fn) s/Any})

(def option-defaults
  (s/validate Options {:ignore-missing-mappings? false
                       :default-response-description-fn (constantly "")
                       :handle-duplicate-schemas-fn rsc/ignore-duplicate-schemas}))

(s/defn swagger-json
  "Produces swagger-json output from ring-swagger spec.
   Optional second argument is a options map, supporting
   the following options with defaults:

   :ignore-missing-mappings?        - (false) boolean whether to silently ignore
                                      missing schema to JSON Schema mappings. if
                                      set to false, IllegalArgumentException is
                                      thrown if a Schema can't be presented as
                                      JSON Schema.

   :default-response-description-fn - ((constantly \"\")) - a fn to generate default
                                      response descriptions from http status code.
                                      Takes a status code (Int) and returns a String.

   :handle-duplicate-schemas-fn     - (ring.swagger.core/ignore-duplicate-schemas),
                                      a function to handle possible duplicate schema
                                      definitions. Takes schema-name and set of found
                                      attached schema values as parameters. Returns
                                      sequence of schema-name and selected schema value.

   :collection-format               - Sets the collectionFormat for query and formData
                                      parameters.
                                      Possible values: multi, ssv, csv, tsv, pipes."
  ([swagger :- (s/maybe Swagger)] (swagger-json swagger nil))
  ([swagger :- (s/maybe Swagger), options :- (s/maybe Options)]
    (let [options (merge option-defaults options)]
      (binding [jsons/*ignore-missing-mappings* (true? (:ignore-missing-mappings? options))]
        (let [[paths definitions] (-> swagger
                                      ensure-body-and-response-schema-names
                                      (extract-paths-and-definitions options))]
          (deep-merge
            swagger-defaults
            (-> swagger
                (assoc :paths paths)
                (assoc :definitions definitions))))))))
