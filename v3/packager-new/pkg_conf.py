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

glassfish_version="10.0.0,0-"
felix_version="10.0.0,0-"
javadb_version="10.0.0,0-"

#description
glassfish_description="GlassFish v3 Application Server"
glassfish_description_log="GlassFish is a free, open source application server."
glassfish_info_classification="Application Servers"
