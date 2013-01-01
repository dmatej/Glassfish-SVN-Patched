#!/bin/sh -e
g=${1}
(cd $g; zip original-osgi-modules.zip modules/osgi-jpa-extension.jar modules/autostart/osgi-*.jar)
cp ./osgi-web-container/target/osgi-web-container.jar ./osgi-cdi/target/osgi-cdi.jar ./osgi-jpa/target/osgi-jpa.jar ./osgi-jta/target/osgi-jta.jar ./osgi-ejb-container/target/osgi-ejb-container.jar ./osgi-http/target/osgi-http.jar ./osgi-jdbc/target/osgi-jdbc.jar ./osgi-javaee-base/target/osgi-javaee-base.jar osgi-ee-resources/target/osgi-ee-resources.jar $g/modules/autostart
cp osgi-jpa-extension/target/osgi-jpa-extension.jar $g/modules/
echo All files successfully copied. Original files are backed up in $g/original-osgi-modules.zip
