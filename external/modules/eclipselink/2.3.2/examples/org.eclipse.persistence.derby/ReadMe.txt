Overview
--------

This bundle uses an Equinox extension to the Manifest file to
make a jar available as a bundle by reference, rather than by 
wrapping it inside a bundle.  This provides a way to build EclipseLink
examples that rely on third party (possibly commercial) JDBC drivers
without having to ship those drivers with EclipseLink. 

This bundle contains *NO* classes.  For it to function you must define the
environment variable DERBY_HOME, (e.g., DERBY_HOME=C:\bin\db-derby-10.3.2.1-bin).
In the bundle Manifest DERBY_HOME is used to reference the derbyclient.jar
to make its classes available as if they resided inside the bundle. The 
jar is referenced as "DERBY_HOME\lib\derbyclient.jar".  

Configuration
-------------

DERBY_HOME must be defined either in your environment or in the Eclipse
Application launch configuration on the "Environment" tab.

Note
----

In Eclipse, PDE will give the error:
    "Package 'org.apache.derby.jdbc' does not exist in this plug-in"
This is expected--don't panic.  At runtime, Equinox will resolve the 
jar using the environment variable.
