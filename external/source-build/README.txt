Pre-requisite:
    1)
        - JDK6 (using JAVA_HOME environment variable)
        - ANT 1.7.1 (using ANT_HOME environment variable)

	* available in PATH:
		- svn (subversion)
		- hg (mercurial)
		- git

	* installed and PATH provided (with CLI or user.properties):
		- JDK5
		- maven 2.2.1
		- maven 3

    2)
        Copy "config/user.properties.sample" to "config/user.properties"
        and update it with your environment.
    OR
        Provide the following properties at command line:
            - proxyHost
            - proxyPort
            - mvn3.home
            - mvn2.home
            - JDK_1.5
            - glassfish.dir
            - project.version

Overridable properties:
    1) from cli only:
        - maven.user.home (default: ${basedir}/.m2)
        - scripts.dir (default: ${basedir}/scripts)

    2) from cli and user.properties
        - java.net.user (default: glassfish_sourcebuild)
        - ssh.user.home (default: ${basedir}/.ssh)
        - maven.repo.local (default: ${maven.user.home}/repository)
        - maven.settings.xml (default: ${maven.user.home}/settings.xml)
        - local.maven.ant.tasks.jar (default: ${basedir}/lib/maven-ant-tasks-2.0.10.jar)
        - glassfish.external.dir (default: ${basedir}/external)
        - logs.dir (default: ${basedir}/logs)
        - proxy.test.url (default: http://dlc.sun.com.edgesuite.net)

Commons use cases:
    - re-build from scratch:
    $ ant clean.all build
    - build without checkout:
    $ ant build -DskipCheckout=true
    - re-build without checkout:
    $ ant clean build -DskipCheckout=true
    - re-build a module (without it's dependencies):
    $ ant -f script/[scriptfile] build -DskipCheckout=true
    - rebuild a module with dependencies
    $ ant [scriptTarget] -Dtarget=build -DskipCheckout=true

Ant targets:
    1) main
	- help (print this screen)
        - clean (to re-build)
        - clean.all (deletes external directory and maven repo)
        - checkout (checkout all modules)
        - build (checkout and build all modules)

    2) others
        - print.requirements
        - print.usage
        - print.proxy-configuration
