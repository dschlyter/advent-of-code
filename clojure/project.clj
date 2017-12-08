(defproject
  advent-of-code
  "1.0"
  :repositories
  [["clojars" {:url "https://clojars.org/repo/"}]
   ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [onetom/boot-lein-generate "0.1.3" :scope "test"]
   [walmartlabs/datascope "0.1.1"]]
  :source-paths
  ["src/clj"]
  :resource-paths
  [])