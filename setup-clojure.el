(require 'package)

(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

(unless (package-installed-p 'cider)
  (package-refresh-contents)
  (package-install 'cider))

;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(provide 'setup-clojure)
