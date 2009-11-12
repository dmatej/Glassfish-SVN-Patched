This is the root of a multi-module maven project, the execution of which
causes the JSR-303 implementation from JBoss (along with the source code
for all the projects on which their implementation depends) to be
checked out from their anonymous source repositories, built and packaged
into a Glassfish v3 OSGi module.

The svn-helpers submodule contains a pom that serves as the parent pom
for dependencies.  This parent pom has logic that eases the process of
checking out and building code from source.
