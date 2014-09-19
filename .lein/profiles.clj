{:user {:plugins [[cider/cider-nrepl "0.7.0"]
                  [jonase/eastwood "0.1.3"]
                  [lein-ancient "0.5.5"]
                  [lein-cljsbuild "1.0.3"]
                  [lein-cloverage "1.0.2"]
                  [lein-kibit "0.0.8"]
                  [lein-release "1.0.5"]
                  [lein-try "0.4.1"]
                  [lein-vanity "0.2.0"]]
        :dependencies [[clojure-complete "0.2.3"]
                       [criterium "0.4.3"]
                       [pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
