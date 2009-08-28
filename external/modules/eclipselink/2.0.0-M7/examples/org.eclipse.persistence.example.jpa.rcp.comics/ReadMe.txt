Introduction
------------

This example provides a simple RCP application that uses EclipseLink JPA.

Setup
-----

It's assumed that the user of this example is a developer with access
to the EclipseLink SVN repository.  See the Eclipse website for instructions on
access.  

You can work with either EclipseLink bundle sources available from SNV, or 
with the EclipseLink compiled bundles.  Since we're just focusing on the
examples, instructions below are for the compiled EclipseLink bundles.

1. Download the EclipseLink bundles (plugins) from the EclipseLink
download page and place them into your Eclipse 3.4 (Ganyemede) 
<ECLIPSE_HOME>\dropins folder.  Be careful to place the bundles *directly*
into dropins and not into "dropins\plugins".

2. Define the variable DERBY_HOME in your environment so that it is available
when running Java applications.  The org.eclipse.persistence.derby bundle (which
we'll come to) expects to find the Derby 10.3.2.1 (or above) derbyclient.jar
in DERBY_HOME\lib.  

3. Start, or restart Eclipse to ensure it sees the dropin bundles.

4. Connect to SVN to obtain the required bundles.  The 1.0 Examples are in:
https://dev.eclipse.org/svnroot/rt/org.eclipse.persistence/branches/1.0/examples

5. Checkout the following PDE projects into your workspace:
	a) org.eclipse.persistence.derby
	b) org.eclipse.persistence.example.jpa.comics.model.annotated
	c) org.eclipse.persistence.example.jpa.comics.setup
	d) org.eclipse.persistence.example.jpa.rcp.comics

6. After everything is compiled you should see the error:
    "Package 'org.apache.derby.jdbc' does not exist in this plug-in"
This is expected and is not an issue. At runtime, Equinox will resolve the 
jar using the environment variable you defined in step 2.

Running the Example
-------------------

1. Start the Derby server using "startNetworkServer[.bat]" from your Derby install.

2. Create and populate the database using the "ComicsExampleDBSetup" launch configuration
in the org.eclipse.persistence.example.jpa.comics.setup project.  When run you should
see various DDL statements in the console along with a number of INSERTS using parameter
binding.

3. Start the Comics RCP application using the "ComicsRCP" launch configuration.  Depending
on your Eclipse environment settings, you may be warned that org.eclipse.persistence.derby
has errors but proceed anyway.

4. With the application open you should be able to browse the database.  Notice that the
tree is lazily populated and that as you dig down the necessary SQL is generated.