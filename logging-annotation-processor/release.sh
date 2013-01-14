#!/bin/sh

#
# Usage:
#   ./release.sh your-jvnet-password
#
#------------------------------------------------------   
#--	BE SURE TO HAVE THE FOLLOWING IN YOUR SETTINGS.XML
#------------------------------------------------------
#
#    <servers>
#        <server>
#            <id>jvnet-nexus-staging</id>
#            <username>jvnet_id</username>
#            <password>password</password>
#        </server>
#    </servers>

if [ "$#" -ne 2 ] || [ "$#" -ne 0 ]
then
  echo "Usage: $0 your-jvnet-username your-jvnet-password"
  exit 1
fi

if [ "$#" -eq 2 ]
then
  CREDENTIALS="-Dusername=$1 -Dpassword=$2"
fi

# see the following URL for gpg issues
# https://docs.sonatype.org/display/Repository/How+To+Generate+PGP+Signatures+With+Maven#HowToGeneratePGPSignaturesWithMaven-GenerateaKeyPair

mvn -B release:prepare -DpreparationGoals="install -P\!jvnet-release" $CREDENTIALS
mvn -B release:perform -Dgoals="deploy -Dhttps.proxyHost=inet-rmmc01.oracle.com -Dhttps.proxyPort=80 -Pjvnet-release -Dgpg.passphrase=glassfish" $CREDENTIALS

