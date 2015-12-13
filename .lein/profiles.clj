{:user {:plugins [[jonase/eastwood "0.2.2"]
                  [lein-ancient "0.6.8" :exclusions [org.clojure/clojure commons-codec commons-logging]]]}
 :repl {:plugins [[cider/cider-nrepl "0.10.0"]
                  [refactor-nrepl "2.0.0-SNAPSHOT" :exclusions [org.clojure/clojure]]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.nrepl "0.2.12"]]}}
