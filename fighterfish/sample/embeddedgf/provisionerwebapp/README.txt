How to build?
-------------
You can use Maven 2.2.1 or Maven 3. Just run:
mvn clean package
It will create target/sample.embeddedgf.provisionerwebapp.war

Prerequisite to use this application:
-------------------------------------
0. You must have installed some other container like WLS or Tomcat and
deployed the earlier webapp called sample.embeddedgf.webosgibridge.war.

1. You must also add simple-glassfish-api.jar to the system classpath of the host container.
This step is required if you want other applications deployed in the host container to use GlassFish.

2. You must define a property called com.sun.aas.installRoot in system or in osgi.properties file
to point to your glassfish installation. You can also decide to configure this web app using a runtime
deployment descriptor or deployment plan if you chose to. Look at the environment entry called gfHome in this webapp.

How to use this to embed GlassFish inside another web container
---------------------------------------------------------------
0. cp target/sample.embeddedgf.provisionerwebapp.war ${DOMAIN_HOME}/autodeploy/
This web app does the following:
Gets hold of the OSGi framework created by earlier web app and provisions GlassFish modules inside that.

It then binds the org.glassfish.embeddable.GlassFish object to JNDI under java:global/glassfish-instance.

1. Now you can run commands like "nadmin version" from another terminal to verify GlassFish is actually started.
You can also deploy other applications in the host container and access GlassFish using GlassFish embeddable API.

2. To restart embedded glassfish, just touch the war file in autodeploy/.