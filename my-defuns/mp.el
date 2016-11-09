(defun mbj/mp (base-dir group-id artifact-id version)
  "creates a maven project"
  (interactive "Dbase-dir:\nsgroup-id:\nsartifact-id:\nsversion (1.0.0-SNAPSHOT):")
  (let* ((package-path (s-replace-all '(("." . "/")) group-id))
         (template "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
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
         (theVersion (if (s-blank? (s-trim version))
                         "1.0.0-SNAPSHOT"
                       version))
         (pom-content (s-replace-all (list (cons "###groupId###" group-id) (cons "###artifactId###" artifact-id) (cons "###version###" theVersion)) template))
         (out-dir (concat base-dir "/" artifact-id))
         (pom-file (concat out-dir "/pom.xml")))
    (mkdir out-dir t)
    (with-temp-buffer
      (insert pom-content)
      (when (file-writable-p pom-file)
        (write-region (point-min)
                      (point-max)
                      pom-file)))
    (mkdir (concat out-dir "/src/main/java/" package-path) t)
    (mkdir (concat out-dir "/src/main/resources/" package-path) t)
    (mkdir (concat out-dir "/src/test/java/" package-path) t)
    (mkdir (concat out-dir "/src/test/resources/" package-path) t)))


;; (mp "/Users/ei4577/slask/git" "com.test" "my-funny-project" "1.0.0-SNAPSHOT")

