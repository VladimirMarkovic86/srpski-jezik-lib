(defproject srpski_jezik "0.1.0-SNAPSHOT"
  :description "Srpski jezik"
  :url "http://gitlab:1610/VladimirMarkovic86/srpski-jezik"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.vladimir/utils-lib "0.1.0"]
                 ]
  
  :main ^:skip-aot srpski-jezik.padezi
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
