{:user {:plugins [[jonase/eastwood "0.2.1"]
                  [lein-ancient "0.6.7" :exclusions [org.clojure/clojure commons-codec commons-logging]]]}
 :repl {:plugins [[cider/cider-nrepl "0.9.1"]
                  [refactor-nrepl "1.1.0"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.nrepl "0.2.10"]]}}
