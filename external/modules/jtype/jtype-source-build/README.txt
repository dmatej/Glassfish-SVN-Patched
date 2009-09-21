This maven project performs a source checkout from a version control
tag, and build of the checked out source.  One or more subprojects
withinn this project inherit from a pom within the
bean-valiator-source-build project.  Therefore, you can't build this
project without first building the bean-validator-source-build project
so that pom gets populated to your local repository.
