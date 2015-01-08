;; =============================================================
;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(package-install 'restclient)
(package-install 'golden-ratio)
(package-install 'kaesar)
