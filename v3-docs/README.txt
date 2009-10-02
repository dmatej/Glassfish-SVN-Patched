To build v3-docs
*   Generate and bundle docs.
    mvn install
    
Note - The step to build the JavaHelp plugin by running mvn -P plugin install is no longer required as the online help is not in the JavaHelp format.

To Release and Promote v3-docs Artifacts

1. mvn -Dmaven.repo.local=<repo> release:prepare -B  -Dtag=<tag-name> -DreleaseVersion=<releasen version> -DdevelopmentVersion=3.0-SNAPSHOT 

(use the tag name v3-docs-3.0-b##):

2.  mvn -Dmaven.repo.local=<repo> -s settings.xml release:perform
