(ns leiningen.rewrite
  (:import [java.io.PushbackReader]
           (java.io PushbackReader))
  (:require
    [clojure.pprint :as pprint]
    [clojure.java.io :as jio]
    [leiningen.core.eval :as eval]
    [lein-repo.plugin :as plugin]))

(defn- find-project-files
  [root]
  (->> root
    jio/as-file
    file-seq
    (filter (fn [file] (= "project.clj" (.getName file))))))

(defn read-project-edn
  [project-clj]
  (with-open [reader (PushbackReader. (jio/reader project-clj))]
    (read reader)))

(defn get-from-proj
  [project-edn key]
  (some (fn [[k v]]
          (when (= k key)
            v))
    (map vector project-edn (rest project-edn))))

(defn set-in-proj
  [project-edn key value]
  (if (get-from-proj project-edn key)
    (cons 'defproject
      (map (fn [[k v]]
             (if (= k key)
               value
               v))
        (map vector project-edn (rest project-edn))))
    (concat project-edn [key value])))

(defn qualify-dep
  [dep]
  (let [spec (get-in plugin/repo-config ['external-dependencies dep])]
    (assert spec (str "Missing external dep " dep))
    spec))

(defn updated-project-edn
  [^java.io.File project-clj]
  (let [project-edn (read-project-edn project-clj)]
    (when-let [deps (get-from-proj project-edn :external-dependencies)]
      (let [qualified (map qualify-dep deps)
            combined (vec (concat qualified (get-from-proj project-edn :dependencies)))]
        (set-in-proj project-edn :dependencies combined)))))

(defn backup-file
  [^java.io.File project-clj]
  (java.io.File. ^java.io.File (.getParentFile project-clj)
    ^String (str (.getName project-clj) ".rewrite-backup")))

(defn move-file
  [^java.io.File source ^java.io.File target destroy?]
  (when-not destroy?
    (when (.exists target)
      (throw (ex-info (str (.getCanonicalPath target) " exists, halting.") {}))))
  (jio/delete-file target (not destroy?))
  (.renameTo source target))

(defn rewrite-project-file
  [^java.io.File project-clj]
  (let [canonical-path (.getCanonicalPath project-clj)]
    (println "Inspecting" canonical-path)
    (when-let [updated (updated-project-edn project-clj)]
      (println "Rewriting" canonical-path)
      (move-file project-clj (backup-file project-clj) false)
      (spit project-clj (with-out-str (pprint/pprint updated))))))

(defn restore-project-file
  [^java.io.File project-clj]
  (let [backup (backup-file project-clj)]
    (when (.exists backup)
      (println "Restoring" (.getCanonicalPath project-clj))
      (move-file backup project-clj true))))

(defn rewrite
  "Rewrite lein-repo project files using explicit references."
  [project & args]
  (if-let [repo-root plugin/repo-root]
    (run! (if (not= ["undo"] args)
            rewrite-project-file
            restore-project-file)
      (find-project-files repo-root))
    (println "Not in a lein-repo monorepo.")))
