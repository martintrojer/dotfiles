{:user {:plugins [[cider/cider-nrepl "0.8.2"]
                  [refactor-nrepl "1.0.4"]
                  [lein-ancient "0.6.7"]
                  [lein-cloverage "1.0.3"]
                  [lein-try "0.4.3"]]
        :dependencies [[criterium "0.4.3"]
                       [pjstadig/humane-test-output "0.7.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
