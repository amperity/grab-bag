(ns leiningen.clean-all
  "Clean all internal projects."
  (:require [leiningen.core.eval :as eval]
            [leiningen.clean :as clean]
            [lein-repo.plugin :as plugin]))

(defn clean-all [project]
  (doseq [project (map plugin/read-project-file (plugin/all-internal-deps))]
    (clean/clean project)))
