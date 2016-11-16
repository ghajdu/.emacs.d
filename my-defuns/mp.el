(defvar mbj/pom-template "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">
    <modelVersion>4.0.0</modelVersion>
    <groupId>###groupId###</groupId>
    <artifactId>###artifactId###</artifactId>
    <version>###version###</version>
    <name>${project.groupId}:${project.artifactId}</name>
    <description>${project.artifactId}</description>    
    <packaging>jar</packaging>
    <properties>
        <maven.compiler.source>1.8</maven.compiler.source>
        <maven.compiler.target>1.8</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.3</version>
                <configuration>
                    <source>${maven.compiler.source}</source>
                    <target>${maven.compiler.target}</target>
                </configuration>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-enforcer-plugin</artifactId>
                <version>1.3.1</version>
                <executions>
                    <execution>
                        <id>enforce-java</id>
                        <goals>
                            <goal>enforce</goal>
                        </goals>
                        <configuration>
                            <rules>
                                <requireJavaVersion>
                                    <version>${maven.compiler.target}</version>
                                </requireJavaVersion>
                            </rules>
                        </configuration>
                    </execution>
                </executions>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-javadoc-plugin</artifactId>
                <version>2.10.3</version>
                <configuration>
                    <show>protected</show>
                    <nohelp>true</nohelp>
                </configuration>
                <executions>
                    <execution>
                        <goals>
                            <goal>jar</goal>
                        </goals>
                    </execution>
                </executions>                
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-source-plugin</artifactId>
                <version>2.4</version>
                <executions>
                    <execution>
                        <goals>
                            <goal>jar</goal>
                        </goals>
                    </execution>
                </executions>                
            </plugin>
        </plugins>
    </build>    
</project>
")

(defun mbj/write-string (file content)
  (with-temp-buffer
    (insert content)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

(defun mbj/create-git-ignore (dir)
  "Creates a .gitignore file."
  (interactive "Ddir:")
  (mbj/write-string (concat dir ".gitignore")
                    (s-join "\n" '(".settings" "/.project" "/.classpath" ".DS_Store" ".idea" "*.iml" "*.iws" "docs.txt"))))

(defun mbj/git-init (dir)
  "Initializes a git repository with a .gitignore file and master and develop branches."
  (interactive "D")
  (mbj/create-git-ignore dir)
  (shell-command "git init")
  (shell-command "git add .gitignore")
  (shell-command "git commit -am \"initial version\"")
  (shell-command "git branch develop")
  (shell-command "git checkout develop")
  (shell-command "git add --all")
  (shell-command "git commit -am \"initial develop version\""))

(defun mbj/mp (base-dir group-id artifact-id version)
  "Creates a maven project and a git repository."
  (interactive "Dbase-dir:\nsgroup-id:\nsartifact-id:\nsversion (1.0.0-SNAPSHOT):")
  (let ((version (if (s-blank? (s-trim version))
                     "1.0.0-SNAPSHOT"
                   version)))
    (let ((out-dir (concat base-dir "/" artifact-id "/"))         
          (package-path (s-replace-all '(("." . "/")) group-id))                 
          (pom-content (s-replace-all (list (cons "###groupId###" group-id)
                                            (cons "###artifactId###" artifact-id)
                                            (cons "###version###" version))
                                      mbj/pom-template)))
      (mkdir out-dir t)
      (mbj/write-string (concat out-dir "pom.xml") pom-content)
      (dolist (dir '("src/main/java/" "src/main/resources/" "src/test/java/" "src/test/resources/"))
        (mkdir (concat out-dir dir package-path) t))
      (let ((default-directory out-dir))
        (mbj/git-init out-dir)))))

(provide 'mbj/mp)


