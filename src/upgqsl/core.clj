(ns upgqsl.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn add-raw-data [state line]
  (if (:current-fn state)
    (update-in state [:current-fn :content] conj line)
    (update-in state [:contents] conj {:type :raw :content line})))

(defn state-xfr [state line]
  (let [dcl-regex #"^\s*((static|final|private|public|protected)\s+)+([^ ]+)\s+([^ \t\(;]+)\s*(.)"]
    (if-let [ms (re-find dcl-regex line)]
      (let [[ftype fname look-ahead] (drop (- (count ms) 3) ms)
            fn-dcl {:type :fn
                    :fn-type ftype
                    :fn-name fname
                    :content [line]}]
        (if (= "(" look-ahead)
          (if (:current-fn state)
            {:contents (conj (:contents state) (:current-fn state))
             :current-fn fn-dcl}
            (assoc state :current-fn fn-dcl))
          (add-raw-data state line)))
      (add-raw-data state line)))
  )

(defn process-class [lines]
  (let [result (reduce state-xfr
                       {:contents [] :current-fn nil}
                       lines)]
    (conj (:contents result) (:current-fn result))))

(defn extract-new-jpaquerys [fn-decl-lines]
  (let [line-with-idx (map vector fn-decl-lines (range))]
    (reduce (fn [ids [line idx]]
              (if-let [[_ _ var-name] (re-find #"(JPAQuery\s+([a-zA-Z0-9_]+)\s*=\s*)?new\s+JPAQuery\(" line)]
                (conj ids {:var-name var-name
                           :var-decl-line-idx idx
                           :var-decl-line line})
                ids))
            []
            line-with-idx))
  )

(defn extract-var-decl [var-name ref-line-idx fn-decl-lines]
  (let [decl-regex (re-pattern (str "^\\s*([^ \t]+)\\s*" var-name "\\s*(=.*)?;"))
        ev (->> fn-decl-lines
                (take ref-line-idx)
                reverse
                (map #(second (re-find decl-regex %)))
                (filter identity)
                first)]
    (if (nil? ev)
      (if (re-find #"," var-name)
        "Tuple"
        (let [[f t] (map #(apply str %) (split-at 1 var-name))]
          (str (string/upper-case f) t)))
      ev)))

(defn extract-ref-stmts [fn-decl-lines offset]
  (let [ref-stmts (:content (reduce (fn [result line]
                                      (cond
                                        (:finished result) result
                                        (re-find #"(;|\{)" line) {:finished :true
                                                             :content (conj (:content result) line)}
                                        :else (update-in result [:content] conj line)))
                                    {:finished nil
                                     :content []}
                                    (drop offset fn-decl-lines)))
        [from-elm] (->> ref-stmts
                        (map #(re-find #"\.\s*from\s*\((.*?)\)" %))
                        (filter identity)
                        (map rest)
                        first)
        
        [fetch-type select-type] (->> ref-stmts
                                      (map #(re-find #"\.\s*(list|singleResult|uniqueResult|count)\s*\((.*?)\)" %))
                                      (filter identity)
                                      (map rest)
                                      first)
        xfred-fetch-type (-> fetch-type
                             keyword
                             {:list "fetch"
                              :singleResult "fetchOne"
                              :uniqueResult "fetchUnique"
                              :count "fetchCount"})
        xfred-stmts (map #(string/replace
                           %
                           #"\.\s*(list|singleResult|uniqueResult|count)\s*\((.*?)\)"
                           (str "." xfred-fetch-type "()"))
                         ref-stmts)]
    {:ref-line-idx offset
     :ref-stmts ref-stmts
     :xfred-stmts (vec xfred-stmts)
     :fetch-type fetch-type
     :select-type (if (= fetch-type "count")
                    from-elm
                    select-type)
     :from-elm from-elm}))

(defn extract-assign-type [var-name ref-line-idx fn-decl-lines]
  (let [line (get fn-decl-lines ref-line-idx)
        usage-regex (re-pattern (str "\\s*([^ \t\\(]*)?\\s+([a-zA-Z0-9]+)\\s*(:|=)\\s*(\\([^\\)]*\\))?\\s*" var-name "\\s*\\."))
        [_ ret-type ret-var] (re-find usage-regex line)]
    (if (or (nil? ret-type) (= "" ret-type))
      (extract-var-decl ret-var ref-line-idx fn-decl-lines)
      ret-type)))

(defn strip-list-wrap [type-decl]
  (if-let [[_ _ raw-type] (re-find #"(List|Iterator)\s*<(.*)>" type-decl)]
    raw-type
    type-decl))

(defn extract-all-refs [fn-decl-lines jpa-var-defs fn-type]
  (let [line-with-idx (map vector fn-decl-lines (range))
        var-alters (string/join "|" (map :var-name jpa-var-defs))
        usage-regex (re-pattern (str "(return|:|=)\\s*(\\([^\\)]*\\))?\\s*(" var-alters ")\\s*\\."))
        var-refs (->> line-with-idx
                      (map (fn [[line idx]]
                             (let [[_ assign-type _ var-name] (re-find usage-regex line)
                                   ]
                               {:var-name var-name
                                :assign-type assign-type
                                :ref-line-idx idx})))
                      (filter :var-name))]
    (map (fn [vr]
           (let [ret-type (if (= (:assign-type vr) "return")
                            fn-type
                            (extract-assign-type (:var-name vr)
                                                 (:ref-line-idx vr)
                                                 fn-decl-lines))]
             (assoc vr
                    :assign-type (:assign-type vr)
                    :ret-type (strip-list-wrap ret-type))))
         var-refs)))

(defn replace-select-type [line select-type]
  (string/replace line
                  #"new\s+JPAQuery\s*\((.*?)\)(.*)"
                  (str "new JPAQueryFactory(" "$1" ").select(" select-type ")" "$2")))

(defn resolve-var-defs [jpa-var-def all-var-refs fn-decl-lines]
  (let [var-refs (->> all-var-refs
                      (filter #(= (:var-name %) (:var-name jpa-var-def)))
                      (map #(merge (extract-ref-stmts fn-decl-lines (:ref-line-idx %)) %)))
        first-ref (-> var-refs first)
        ret-type (if (= "count" (:fetch-type first-ref))
                   (extract-var-decl (:from-elm first-ref)
                                     (:ref-line-idx first-ref)
                                     fn-decl-lines)
                   (:ret-type first-ref))
        xfred1 (string/replace (:var-decl-line jpa-var-def)
                               (re-pattern (str "JPAQuery\\s+" (:var-name jpa-var-def) "\\s+="))
                               (str "JPAQuery<" ret-type
                                    "> " (:var-name jpa-var-def)
                                    " ="))
        xfred2 (replace-select-type xfred1 (:select-type first-ref))]
    (assoc jpa-var-def
           :refs var-refs
           :xfred-decl-line xfred2)))

(defn process-fn [fn-decl]
  (let [new-jpaquerys (extract-new-jpaquerys (:content fn-decl))
        jpa-var-defs (filter :var-name new-jpaquerys)
        direct-uses (filter (comp not :var-name) new-jpaquerys)
        all-var-refs (if (> (count jpa-var-defs) 0)
                       (extract-all-refs (:content fn-decl)
                                         jpa-var-defs
                                         (:fn-type fn-decl)))
        jpa-vars (map (fn [jvd]
                        (resolve-var-defs jvd all-var-refs (:content fn-decl)))
                      jpa-var-defs)
        direct-uses1 (map (fn [jvd]
                            (let [uses (extract-ref-stmts (:content fn-decl)
                                                          (:var-decl-line-idx jvd))
                                  xfred (update-in uses [:xfred-stmts 0]
                                                   replace-select-type
                                                   (:select-type uses))]
                              (assoc jvd :refs [xfred])))
                          direct-uses)]
    (->> (concat jpa-vars direct-uses1)
         (group-by :var-decl-line-idx)
         (map (comp first val)))))

(defn prepare-replace-info [jpa-vars]
  (let [coll (mapcat (fn [jv]
                       (let [xfred (map (fn [jv-ref]
                                          {:idx (:ref-line-idx jv-ref) :stmts (:xfred-stmts jv-ref)})
                                        (:refs jv))]
                         (if (:xfred-decl-line jv)
                           (cons {:idx (:var-decl-line-idx jv)
                                  :stmts [(:xfred-decl-line jv)]}
                                 xfred)
                           xfred)))
                     jpa-vars)
        coll2 (->> coll
                   (group-by :idx)
                   (map (comp first val)))]
    (sort #(< (:idx %1) (:idx %2)) coll2)))

(defn replace-fn-content [fn-decl-lines jpa-vars]
  (let [merged (reduce (fn [xfred jv]
                         {:lines (vec (concat (:lines xfred)
                                              (subvec fn-decl-lines (:prev-idx xfred) (:idx jv))
                                              (:stmts jv)))
                          :prev-idx (+ (:idx jv) (count (:stmts jv)))})
                       {:lines [] :prev-idx 0}
                       jpa-vars)]
    (concat (:lines merged) (subvec fn-decl-lines (:prev-idx merged)))))

(defn format-output [state]
  (reduce (fn [out content]
            (if (= (:type content) :fn)
              (let [p1 (process-fn content)
                    replace-info (prepare-replace-info p1)
                    xfed (replace-fn-content (:content content) replace-info)]
                (concat out xfed))
              (conj out (:content content))))
          []
          state))

(defn file-format [fname options]
  (with-open [rdr (io/reader fname :encoding (:encoding options))]
    (let [content (slurp rdr)]
      (if (re-find #"\r" content) "DOS" "UNIX"))))

(defn upgrade-file [in-name out-name options]
  (let [ff (file-format in-name options)
        line-separator (if (= ff "DOS") "\r\n" "\n")]
    (with-open [rdr (io/reader in-name :encoding (:encoding options))
                wtr (io/writer out-name :encoding (:encoding options))]
      (let [lines (line-seq rdr)
            result (process-class lines)
            xfred (format-output result)
            xfred1 (->> xfred
                        (filter identity)
                        (map (fn [line]
                               (string/replace line
                                               #"import com.mysema.query.jpa.impl.JPAQuery;"
                                               "import com.querydsl.jpa.impl.JPAQuery;\nimport com.querydsl.jpa.impl.JPAQueryFactory;")))
                        vec)]
        (binding [*out* wtr]
          (print (string/join line-separator (conj xfred1 nil))))
        ))))

(def cli-options
  [["-e" "--encoding ENCODING" "character encoding"
    :default "UTF-8"]
   ["-f" "--format FORMAT" "file format"
    :default "UNIX"]])

(defn -main [& args]
  (let [cmd-line (parse-opts args cli-options)
        options (:options cmd-line)
        [src dst] (:arguments cmd-line)]
    (println "converting" src)
    (upgrade-file  src dst options)
    ))
