{:user {:plugins [[jonase/eastwood "0.2.3"]
                  [lein-ancient "0.6.10" :exclusions [org.clojure/clojure commons-codec commons-logging]]]}
 :repl {:plugins [[cider/cider-nrepl "0.13.0"]
                  [refactor-nrepl "2.2.0" :exclusions [org.clojure/clojure]]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [criterium "0.4.4"]]}}
