Introduction:
-------------
This is a simple web application that boots an OSGi framework inside an appserver and provisions
bundles located in WEB-INF/bundles/ dir of the web app. Those bundles include Felix Remote
Shell bundles, so once the web app is up & running, one can connect to the Felix shell and
pretty much control everything. The remote shell listens on port 6666 by default.
The web app also binds the Felix Framework in JNDI under java:global/glassfish-osgi-framework, so
some other application can locate it and use the same.

The program tries to read OSGi configuration using the following logic:
It has an environment entry defined like this:
    @Resource
    public void setOsgiFrameWorkJndiName(String osgiFrameWorkJndiName) {
        this.osgiFrameWorkJndiName = osgiFrameWorkJndiName;
    }

So, while deploying this app, you can supply the value using some sort of deployment plan.
If no value is provided, it defaults to "osgi.properties" which it will look for relative to
current working directory. 

If no such resource is found, it uses the embedded osgi configuration located in WEB-INF/osgi.properties
of the war file.

The OSGi cache is located in /tmp/osgi-cache/ by default.
You can modify it by setting appropriate value in 
org.osgi.framework.storage property in your osgi.properties file.
The framework is configured to forget its state by default. 
You can modify it by setting appropriate value in 
org.osgi.framework.storage.clean property in your osgi.properties file.

How to build?
-------------
You can use Maven 2.2.1 or Maven 3. Just run:
mvn clean package
It will create target/sample.embeddedgf.webosgibridge.war

How to use this
---------------
1. Setup Tomcat or WebLogic or something else.
running the steps below.
2. cp target/sample.embeddedgf.webosgibridge.war ${DOMAIN_HOME}/autodeploy/
The web app does the following:
It starts an OSGi framework and registers it with JNDI under name java:global/glassfish-osgi-framework.
It also starts OSGi gogo shell bundles located in WEB-INF/bundles/ directory of the war file.
SO, after the war is deployed, you can do the following:
telnet localhost 6666
-> lb -l

