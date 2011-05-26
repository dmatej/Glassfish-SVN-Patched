/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */


package org.glassfish.fighterfish.test.it;

import junit.framework.Assert;
import org.glassfish.embeddable.CommandResult;
import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.glassfish.fighterfish.test.util.WebAppBundle;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import static org.junit.Assert.fail;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class AbstractTestObject extends CommonConfiguration {
    /**
     * List of bundles installed by a test method
     */
    protected List<Bundle> testBundles = new ArrayList<Bundle>();

    /**
     * List of config changes made by a test method
     */
    protected List<RestorableDomainConfiguration> rdcs = new ArrayList<RestorableDomainConfiguration>();
    private static final String DEFAULT_DS = "jdbc/__default";
    private static final String DEFAULT_POOL = "DerbyPool";
    protected static final File derbyRootDir = getDerbyDBRootDir();

    protected static File getDerbyDBRootDir() {
        String s = System.getProperty("fighterfish.test.DerbyDBRootDir", System.getProperty("java.io.tmpdir"));
        File d = new File(s);
        if (!d.isDirectory() && !d.mkdirs()) {
            throw new RuntimeException("Can't create a directory called " + d.getAbsolutePath());
        }
        return d;
    }

    /**
     * Install a bundle and add it to the list of bundles.
     *
     * @param ctx
     * @param location
     * @throws BundleException
     */
    protected Bundle installTestBundle(BundleContext ctx, String location) throws BundleException {
        final Bundle bundle = ctx.installBundle(location);
        testBundles.add(bundle);
        logger.logp(Level.INFO, "AbstractTestObject", "installTestBundle", "bundle = {0}", new Object[]{bundle});
        return bundle;
    }

    /**
     * Uninstall a bundle if it has been installed by
     *
     * @param bundle
     * @throws BundleException
     */
    protected void uninstallTestBundle(Bundle bundle) throws BundleException {
        if (testBundles.remove(bundle)) {
            bundle.uninstall();
            logger.logp(Level.INFO, "AbstractTestObject", "uninstallTestBundle", "bundle = {0}", new Object[]{bundle});
        } else {
            throw new RuntimeException(bundle + " is not a test bundle");
        }
    }

    protected void uninstallAllTestBundles() throws BundleException {
        // take a copy because uninstallTstBundle removes from this list
        for (Bundle b : testBundles.toArray(new Bundle[0])) {
            uninstallTestBundle(b);
        }
    }

    protected void restoreDomainConfiguration() throws GlassFishException {
        for (RestorableDomainConfiguration rdc : rdcs) {
            try {
                rdc.restore();
            } catch (GlassFishException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Configures jdbc/__default datasource to use a custom pool that uses embedded derby.
     */
    protected RestorableDomainConfiguration configureEmbeddedDerby(final GlassFish gf, final String poolName, File db) throws GlassFishException {
//        CommandResult result = gf.getCommandRunner().run("set",
//                "resources.jdbc-connection-pool.DerbyPool.datasource-classname=" +
//                        "org.apache.derby.jdbc.EmbeddedXADataSource");
//        logger.logp(Level.INFO, "AbstractTestObject", "configureEmbeddedDerby",
//                "asadmin set command returned: {0}", new Object[]{result.getOutput()});
//        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
//            Assert.fail(result.getOutput());
//        }
        createDerbyPool(gf, poolName, db);
        CommandResult result = gf.getCommandRunner().run("delete-jdbc-resource", DEFAULT_DS);
        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
            Assert.fail(result.getOutput());
        }
        result = gf.getCommandRunner().run("create-jdbc-resource", "--connectionpoolid", poolName, DEFAULT_DS);
        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
            Assert.fail(result.getOutput());
        }
        final RestorableDomainConfiguration rdc = new RestorableDomainConfiguration() {
            @Override
            public void restore() throws GlassFishException {
                CommandResult result = gf.getCommandRunner().run("delete-jdbc-resource", DEFAULT_DS);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }
                result = gf.getCommandRunner().run("delete-jdbc-connection-pool", poolName);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }
                result = gf.getCommandRunner().run("create-jdbc-resource", "--connectionpoolid", DEFAULT_POOL, DEFAULT_DS);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }

            }
        };
        rdcs.add(rdc);
        return rdc;
    }

    private void createDerbyPool(GlassFish gf, String poolName, File db) throws GlassFishException {
        String dbDir = db.getAbsolutePath();
        if (System.getProperty("os.name", "generic").toLowerCase().startsWith("windows")) {
            // We need to escape : as well as backslashes.
            // So, it should something like like C\:\\temp\\foo
            dbDir = dbDir.replace("\\", "\\\\").replace(":", "\\:");
	      System.out.println("derby dir name = " + dbDir);
        }
        CommandResult result = gf.getCommandRunner().run("create-jdbc-connection-pool",
                "--datasourceclassname=org.apache.derby.jdbc.EmbeddedXADataSource",
                "--restype=javax.sql.XADataSource",
                "--property", "databaseName=" + dbDir+ ":" + "connectionAttributes=" + ";create\\=true",
                poolName);
        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
            Assert.fail(result.getOutput());
        }
    }

    protected RestorableDomainConfiguration createJmsCF(final GlassFish gf, final String cfName) throws GlassFishException {
        final RestorableDomainConfiguration rdc = createJmsResource(gf, cfName, "javax.jms.ConnectionFactory");
        rdcs.add(rdc);
        return rdc;
    }

    protected RestorableDomainConfiguration createJmsTopic(final GlassFish gf, final String topicName) throws GlassFishException {
        final RestorableDomainConfiguration rdc = createJmsResource(gf, topicName, "javax.jms.Topic");
        rdcs.add(rdc);
        return rdc;
    }

    protected RestorableDomainConfiguration createJmsQueue(final GlassFish gf, final String topicName) throws GlassFishException {
        final RestorableDomainConfiguration rdc = createJmsResource(gf, topicName, "javax.jms.Queue");
        rdcs.add(rdc);
        return rdc;
    }

    private RestorableDomainConfiguration createJmsResource(final GlassFish gf, final String resName, final String resType) throws GlassFishException {
        CommandResult result = gf.getCommandRunner().run("create-jms-resource", "--restype",
                resType, resName);
        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
            Assert.fail(result.getOutput());
        }
        return new RestorableDomainConfiguration() {
            @Override
            public void restore() throws GlassFishException {
                gf.getCommandRunner().run("delete-jms-resource", resName);
            }
        };
    }

    protected static String getResponse(WebAppBundle wab, String loginRequest) {
        try {
            return wab.getResponse(loginRequest);
        } catch (IOException e) {
            fail(e.toString());
        }
        return null; // never reaches
    }

    /**
* @author Sanjeeb.Sahoo@Sun.COM
*/
public static class StringPatternMatcher extends BaseMatcher<String> {
    private final String expectedOutputPattern;

    public StringPatternMatcher(String expectedOutputPattern) {this.expectedOutputPattern = expectedOutputPattern;}

    @Override
    public boolean matches(Object o) {
        return String.class.cast(o).contains(expectedOutputPattern);
    }

    @Override
    public void describeTo(Description description) {
        description.appendText("Expected output to contain [" + expectedOutputPattern + "]");
    }
}
}
