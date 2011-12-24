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


package org.glassfish.fighterfish.test.util;

import junit.framework.Assert;
import org.glassfish.embeddable.CommandResult;
import org.glassfish.embeddable.GlassFish;
import org.glassfish.embeddable.GlassFishException;
import org.osgi.framework.BundleContext;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class EnterpriseResourceProvisioner {

    /**
     * List of config changes made by a test method
     */
    protected List<RestorableDomainConfiguration> rdcs = new ArrayList<RestorableDomainConfiguration>();
    private boolean inMemoryDerbyDb;
    private File derbyDbRootDir;

    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    public EnterpriseResourceProvisioner(BundleContext ctx) {
        String derbyDbRootDir = ctx.getProperty(Constants.FIGHTERFISH_TEST_DERBY_DB_ROOT_DIR);
        if (derbyDbRootDir == null || derbyDbRootDir.isEmpty()) {
            inMemoryDerbyDb = true;
        } else {
            this.derbyDbRootDir = new File(derbyDbRootDir);
            if (!this.derbyDbRootDir.isDirectory() && !this.derbyDbRootDir.mkdirs()) {
                throw new RuntimeException("Can't create a directory called " + this.derbyDbRootDir.getAbsolutePath());
            }
        }
    }

    private File getDerbyDBRootDir() {
        return derbyDbRootDir;
    }

    private boolean isInmemoryDerbyDb() {
        return inMemoryDerbyDb;
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
    protected RestorableDomainConfiguration configureEmbeddedDerby(final GlassFish gf, final String poolName, String db) throws GlassFishException {
//        CommandResult result = gf.getCommandRunner().run("set",
//                "resources.jdbc-connection-pool.DerbyPool.datasource-classname=" +
//                        "org.apache.derby.jdbc.EmbeddedXADataSource");
//        logger.logp(Level.INFO, "AbstractTestObject", "configureEmbeddedDerby",
//                "asadmin set command returned: {0}", new Object[]{result.getOutput()});
//        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
//            Assert.fail(result.getOutput());
//        }
        if (isInmemoryDerbyDb()) {
            createPoolForInmemoryDerbyDb(gf, poolName, db);
        } else {
            createPoolForEmbeddedDerbyDb(gf, poolName, db);
        }
        execute(gf, "delete-jdbc-resource", Constants.DEFAULT_DS);
        execute(gf, "create-jdbc-resource", "--connectionpoolid", poolName, Constants.DEFAULT_DS);
        final RestorableDomainConfiguration rdc = new RestorableDomainConfiguration() {
            @Override
            public void restore() throws GlassFishException {
                CommandResult result = gf.getCommandRunner().run("delete-jdbc-resource", Constants.DEFAULT_DS);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }
                result = gf.getCommandRunner().run("delete-jdbc-connection-pool", poolName);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }
                result = gf.getCommandRunner().run("create-jdbc-resource", "--connectionpoolid", Constants.DEFAULT_POOL, Constants.DEFAULT_DS);
                if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
                    Assert.fail(result.getOutput());
                }
            }
        };
        rdcs.add(rdc);
        return rdc;
    }

    /**
     * This method creates a connection pool that uses embedded Derby driver to talk to a directiry based Derby database
     * @param gf GlassFish object
     * @param poolName name of connection pool
     * @param db database name
     * @throws GlassFishException
     */
    private void createPoolForEmbeddedDerbyDb(GlassFish gf, String poolName, String db) throws GlassFishException {
        String dbDir = new File(getDerbyDBRootDir(), db).getAbsolutePath();
        if (System.getProperty("os.name", "generic").toLowerCase().startsWith("windows")) {
            // We need to escape : as well as backslashes.
            // So, it should something like like C\:\\temp\\foo
            dbDir = dbDir.replace("\\", "\\\\").replace(":", "\\:");
        }
        final String poolProps = "databaseName=" + dbDir + ":" + "connectionAttributes=" + ";create\\=true";
        execute(gf,
                "create-jdbc-connection-pool",
                "--ping",
                "--restype=javax.sql.XADataSource",
                "--datasourceclassname=org.apache.derby.jdbc.EmbeddedXADataSource",
                "--property",
                poolProps,
                poolName);
    }

    /**
     * This method creates a connection pool that uses embedded Derby driver to talk to an in-memory Derby database
     * @param gf GlassFish object
     * @param poolName name of connection pool
     * @param db name of the database
     * @throws GlassFishException
     */
    private void createPoolForInmemoryDerbyDb(GlassFish gf, String poolName, String db) throws GlassFishException {
        // According to Derby guide available at
        // http://db.apache.org/derby/docs/10.7/devguide/cdevdvlpinmemdb.html#cdevdvlpinmemdb ,
        // an in-memory databae url is of the form: jdbc:derby:memory:db;create=true
        // The above syntax works if we create a java.sql.Driver type resource, but not with DataSource type.
        String poolProps = "url=jdbc\\:derby\\:memory\\:" + db + ";create\\=true";
        execute(gf,
                "create-jdbc-connection-pool",
                "--ping",
                "--restype=java.sql.Driver",
                "--driverclassname=org.apache.derby.jdbc.EmbeddedDriver",
                "--property",
                poolProps,
                poolName);
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
        execute(gf, "create-jms-resource", "--restype", resType, resName);
        return new RestorableDomainConfiguration() {
            @Override
            public void restore() throws GlassFishException {
                gf.getCommandRunner().run("delete-jms-resource", resName);
            }
        };
    }

    private CommandResult execute(GlassFish gf, String cmd, String... args) throws GlassFishException {
        logger.logp(Level.INFO, "EnterpriseResourceProvisioner", "execute", "cmd = {0}, args = {1}", new Object[]{cmd, Arrays.toString(args)});
        CommandResult result = gf.getCommandRunner().run(cmd, args);
        if (result.getExitStatus() == CommandResult.ExitStatus.FAILURE) {
            Assert.fail(result.getOutput());
        }
        return result;
    }

}
