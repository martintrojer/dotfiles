;; =============================================================
;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

