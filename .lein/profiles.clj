{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [refactor-nrepl "1.0.5"]
                  [lein-ancient "0.6.7" :exclusions [org.clojure/clojure commons-codec commons-logging]]
                  [lein-try "0.4.3"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.nrepl "0.2.10"]
                       ;;[pjstadig/humane-test-output "0.7.0"]
                       ]
        ;; :injections [(require 'pjstadig.humane-test-output)
        ;;              (pjstadig.humane-test-output/activate!)]
        }}
