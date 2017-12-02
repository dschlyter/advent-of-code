(task-options!
 pom {:project     'advent-of-code
      :version     "1.0"
      :description "Advent of code"})

(set-env!
 :source-paths #{"src/clj"}
 :dependencies
 '[
   [org.clojure/clojure "1.8.0"]
   [onetom/boot-lein-generate "0.1.3" :scope "test"]])

; generate project.clj for clojure
(require '[boot.lein])
(boot.lein/generate)

; autorefresh
(merge-env! :dependencies '[[samestep/boot-refresh "0.1.0" :scope "test"]])
(require '[samestep.boot-refresh :refer [refresh]])

; load it in the repl
(require 'advent.advent-2017)
