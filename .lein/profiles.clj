{:user {:plugins [[cider/cider-nrepl "0.8.2"]
                  [refactor-nrepl "0.2.2"]
                  [lein-ancient "0.5.5"]
                  [lein-cloverage "1.0.2"]
                  [com.jakemccrary/lein-test-refresh "0.5.5"]
                  [lein-release "1.1.3"]
                  [lein-try "0.4.3"]]
        :dependencies [[clojure-complete "0.2.4"]
                       [criterium "0.4.3"]
                       [pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
