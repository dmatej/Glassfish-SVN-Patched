#!/bin/sh
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

# see the following URL for gpg issues
# https://docs.sonatype.org/display/Repository/How+To+Generate+PGP+Signatures+With+Maven#HowToGeneratePGPSignaturesWithMaven-GenerateaKeyPair

mvn -B $1 release:prepare -DpreparationGoals="install -P\!jvnet-release"
mvn -B $1 release:perform -Dgoals="deploy $2 -Dgpg.passphrase=glassfish -Pjvnet-release"
