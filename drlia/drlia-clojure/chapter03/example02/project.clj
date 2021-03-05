(defproject clojure-java-mixed "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.deeplearning4j/deeplearning4j-core "1.0.0-beta6"]
                 [org.nd4j/nd4j-native-platform "1.0.0-beta6"]]
  :main ^:skip-aot run
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :source-paths      ["src/clj"]
  :java-source-paths ["src/java"])
