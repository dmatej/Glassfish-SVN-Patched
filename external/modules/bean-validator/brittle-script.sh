# remove the binaries from the origin source tree
find ../bean-validator-source-build -name target -type d -exec rm -rf {} \;
# copy everything else from the origin source tree
cp -r ../bean-validator-source-build/* .
# remove the .svn directories
find . -name .svn -exec rm -rf {} \; -print
# remove some unnecessary directories and files
rm -rf README.txt checkout-source svn-helpers test
# remove the automated checkout directories
find . -name checkout -type d -exec rm -rf {} \; -print
# remove the calls into the automated checkout directories
find . -name pom.xml -exec perl -pi.bak -e 's/.module.checkout..module./ /g' {} \; -print
# remove the call into the svn-helpers directory from the top level pom.xml
perl -pi.bak -e 's/.module.svn-helpers..module./ /g' pom.xml
#find . -name checkout-source -prune -o -name jtype -prune -o -name slf4j-api -prune -o -name bundle -prune -o -name pom.xml -exec perl -pi.bak -e 's/<plugins>/<!--<plugins>/g' {} \; -print
#find . -name checkout-source -prune -o -name jtype -prune -o -name slf4j-api -prune -o -name bundle -prune -o -name pom.xml -exec perl -pi.bak -e 's/<\/plugins>/<\/plugins>-->/g' {} \; -print
rmbak
