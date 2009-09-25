To build v3-docs
*   Generate and bundle docs.
    mvn install
    
Note - The step to build the JavaHelp plugin by running mvn -P plugin install is no longer required as the online help is not in the JavaHelp format.

To Release and Promote v3-docs Artifacts

1.  Create a branch from trunk with name: v3-docs-3.0-b## where ## is the build number.

    svn copy https://svn.dev.java.net/svn/glassfish-svn/trunk/v3-docs https://svn.dev.java.net/svn/glassfish-svn/branches/v3-docs-3.0-b##

2.  Check out from the trunk:

    svn checkout https://svn.dev.java.net/svn/glassfish-svn/branches/v3-docs-3.0-b##
    
3.  cd v3-docs-3.0-b##

4.  Execute maven release plugin (use the tag name v3-docs-3.0-b##):

    mvn -Dmaven.repo.local=<repo> -B -Dtag=<name of tag> release:prepare -DlocalRepoDirectory=<local maven repo> -DreleaseVersion=3.0-b## -DdevelopmentVersion=3.0-SNAPSHOT 

5.  mvn release:perform
