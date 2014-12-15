(defun mbj/java-pojo(indentLevel className properties)
  (defun indent(level)
    (make-string (* 4 level) ?\s))
  
  (defun fields(indentLevel properties)
    (defun field(indentLevel property)
      (let ((type (car property)) (name (car (cdr property))))
        (concat (indent indentLevel) "private " type " " name ";")))
    (if properties
        (concat (field indentLevel (car properties)) "\n" (fields indentLevel (cdr properties)))
      ""))

  (defun getters(indentLevel properties)
    (defun getter(indent property)
      (let ((type (car property)) (name (car (cdr property))))
        (concat (indent indentLevel)  "public " type " get" (capitalize name) "() {\n"
                (indent (+ 1 indentLevel)) "return " name ";\n"
                (indent indentLevel) "}")))
    (if properties
        (concat (getter indentLevel (car properties)) "\n" (getters indentLevel (cdr properties)))
      ""))

  (defun builder(indentLevel className properties)
    (defun propertyNames(properties delimiter)
      (if properties
          (concat delimiter (car (cdr (car properties))) (propertyNames (cdr properties) ", "))
        ""))

    (defun setters(indentLevel className properties)
      (defun setter(indent className property)
        (let ((type (car property)) (name (car (cdr property))))
          (concat (indent indentLevel) "public " className " " name "(" type " " name  ") {\n"
                  (indent (+ 1 indentLevel)) "this." name " = " name ";\n"
                  (indent (+ 1 indentLevel)) "return this;\n"
                  (indent indentLevel) "}")))
      (if properties
          (concat (setter indentLevel className (car properties)) "\n" (setters indentLevel className (cdr properties)))
        ""))    

    (defun build(indentLevel className properties)
      (concat (indent indentLevel) "public " className " build() {\n"
              (indent (+ 1 indentLevel)) "return new " className "(" (propertyNames properties "") ");\n"
              (indent indentLevel) "}\n"))

    (concat (indent indentLevel) "public static class Builder {\n"
            (fields (+ 1 indentLevel) properties)
            (setters (+ 1 indentLevel) "Builder" properties)
            (build (+ 1 indentLevel) className properties)          
            (indent indentLevel) "}"))  

  (defun constructor(indentLevel className properties)
    (defun args(properties delimiter)
      (if properties
          (concat delimiter (car (car properties)) " " (car (cdr (car properties))) (args (cdr properties) ", "))
        ""))

    (defun body(indentLevel properties delimiter)
      (if properties
          (let ((name (car (cdr (car properties)))))
            (concat delimiter (indent indentLevel) "this." name " = " name ";"
                    (body indentLevel (cdr properties) "\n")))
        ""))

    (concat (indent indentLevel) "public " className "(" (args properties "")  ") {\n"
            (body (+ 1 indentLevel) properties "") "\n"
            (indent indentLevel) "}\n"))

  (concat (indent indentLevel) "public class " className " {\n"
          (fields (+ 1 indentLevel) properties)
          (constructor (+ 1 indentLevel) className properties)
          (getters (+ 1 indentLevel) properties)
          (builder (+ 1 indentLevel) className properties) "\n"
          (indent indentLevel) "}"))

(defun mbj/builder()
  "
Insert a java builder pattern class in buffer based on region looking like:
Myclass
String foo
int bar
"
  (interactive)
  (defun parseProperties(propertyLines)
    (if propertyLines
        (cons (split-string (car propertyLines) " ") (parseProperties(cdr propertyLines)))
      '()))  
  (let ((regionLines (filter-buffer-substring (region-beginning) (region-end) t)))
    (setq lines (split-string regionLines "\n" t " +"))
    (setq className (car lines))
    (setq properties (parseProperties (cdr lines)))
    (message className))
  (insert (mbj/java-pojo 0 className properties)))

(provide 'builder-pattern-java)
