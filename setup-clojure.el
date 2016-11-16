(require 'package)

(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

(unless (package-installed-p 'cider)
  (package-refresh-contents)
  (package-install 'cider))

;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Use the same ns for src and test
(setq cider-test-infer-test-ns
      (lambda (ns)
        ns))

(add-hook 'cider-mode-hook (lambda () (cider-auto-test-mode)))

(provide 'setup-clojure)
