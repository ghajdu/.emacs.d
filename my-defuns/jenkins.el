(require 's)

(defvar jenkins/base-url "http://10.46.64.51:8080/job/")

(defun jenkins/create-url (uri)
  "Creates url with uri."
  (interactive "s:uri")
  (concat jenkins/base-url uri))

(ert-deftest test-jenkins/create-url ()
  (should (equal (jenkins/create-url "uri") (concat jenkins/base-url "uri"))))

(defun jenkins/get-config (url)
  "Get config from url."
  (interactive "surl:")
  (with-temp-buffer
    ;; Implement correctly!!!
    (insert-file-contents "/Users/ei4577/git/jenkins/LPLANO_app.getprocessinstances.v1_0_develop/config.xml") 
    (buffer-string)))

;;(jenkins/get-config (jenkins/create-url "LPLANO_app.getprocessinstances.v1_0_1.0.0/config.xml"))

(defun jenkins/create-template (config)
  "Creates a template from the config."
  (interactive "sconfig:")
  (let* ((description (nth 1 (s-match "<description>\\(.*\\)</description>" config)))
         (build (cdr (s-match "\\(.*?\\)_\\(.*\\)_\\(.*\\)" description)))
         (replacements (list (cons (upcase (nth 0 build)) "##PROJECT##")
                             (cons (downcase (nth 0 build)) "##project##")
                             (cons (nth 1 build) "##repository##")
                             (cons (nth 2 build) "##branch##"))))
    (s-replace-all replacements config)))

(ert-deftest test-jenkins/create-template ()
  (should (equal (jenkins/create-template "<description>LPLANO_app.getprocessinstances.v1_0_1.0.0</description>lplano")
                 "<description>##PROJECT##_##repository##_##branch##</description>##project##")))

(defun jenkins/create-config (template project repository branch)
  "Creates a jenkins config based on template."
  (interactive "ftemplate:\nsproject:\nsrepository:\nsbranch:")
  (s-replace-all (list (cons "##PROJECT##" (upcase project))
                       (cons "##project##" (downcase project))
                       (cons "##repository##" repository)
                       (cons "##branch##" branch))
                 template))

(ert-deftest test-jenkins/create-config ()
  (should (equal (jenkins/create-config "<description>##PROJECT##_##repository##_##branch##</description>##project##" "LHAUTO" "foo.bar.baz.v1_0" "develop")
                 "<description>LHAUTO_foo.bar.baz.v1_0_develop</description>lhauto")))

(defun jenkins/create-uri (config)
  "Creates uri from config."
  (interactive "fconfig:")
  (let* ((description (nth 1 (s-match "<description>\\(.*\\)</description>" config))))
    (concat description "/config.xml")))

(ert-deftest test-jenkins/create-uri ()
  (should (equal (jenkins/create-uri "<description>LPLANO_app.getprocessinstances.v1_0_1.0.0</description>lplano")
                 "LPLANO_app.getprocessinstances.v1_0_1.0.0/config.xml")))

(defun jenkins/create-job (config)
  "Creates a job with config."
  (interactive "fconfig:")
  ;; Call jenkins with config
  (let* ((url (jenkins/create-url (jenkins/create-uri config)))
         )
    )
  )

(provide 'jenkins)
