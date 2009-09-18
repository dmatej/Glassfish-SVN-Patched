# version should be in the following format:
# <product-version>,0-<build-number>
#
# We have four types of releases, Major (3.0), Minor (3.1), Update (3.0 UR1) 
# and Patch (3.0 UR1 Patch 1), so we are going to let our version number go 
# upto four digits. However, we will start with 3.0 and add more digits as 
# needed.
#
# As for build-number, we will use three digits. The first two digits will 
# correspond to our well known build numbers with alphabets in build number 
# being translated to the second digit (so build 17 will become 17.0 and 
# build 18a will be 18.1). The third digit will default to 0 and will be 
# used when we start publishing our nightly builds to a package repository. 
# In that case, the number will be incremented by 1 for each nightly build. 
# We will start with 2-digit form and add the third digit when we need it. 
# So, to start with our package versions would look like 3.0,0-18.0.
#
# Now, there are some packages used in GlassFish that have their own 
# well-defined versions (for example, grizzly, Felix, JavaDB etc.) and we 
# will use that. We will not add build numbers for these packages. For 
# example, grizzly version would look like 1.8.2-0,0. 

glassfish_version="3.0,0-28.1"
felix_version="1.8.0,0-0"
javadb_version="10.4.2.1,0-0"
corba_version="3.0.0,0-20"
jsf_version="2.0.0,0-14"
grizzly_version="1.9.15,0-0"
metro_version="2.0,0-14"

#description
glassfish_description="GlassFish v3 Application Server"
glassfish_description_long="GlassFish v3 is the reference implementation of Java EE 6 Platform as defined by JCP (Java Community Process, www.jcp.org). In addition to the latest Java EE standards, GlassFish v3 includes scripting support built on top of the new modular GlassFish kernel running on OSGi platform."
glassfish_info_classification="Application Servers"
