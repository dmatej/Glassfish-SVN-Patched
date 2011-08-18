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

import org.glassfish.embeddable.CommandResult;
import org.glassfish.embeddable.CommandRunner;
import org.glassfish.embeddable.GlassFish;
import org.glassfish.fighterfish.test.util.GlassFishTracker;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.Option;
import org.ops4j.pax.exam.junit.Configuration;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;
import org.osgi.framework.BundleContext;

import java.io.IOException;

import static org.ops4j.pax.exam.CoreOptions.*;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
@Ignore
public class T3_SampleGFProvisionerTest {

    // in ms. set this depending on how long it takes to extract and start glassfish.
    private Long TIMEOUT = 100000l;
    private final String LOCAL_WEB_ZIP = "file:/space/ss141213/.m2/repository/org/glassfish/distributions/web/3.1.1-SNAPSHOT/web-3.1.1-SNAPSHOT.zip";
    private final String REMOTE_NUCLEUS_ZIP = "http://maven.glassfish.org/content/groups/glassfish/org/glassfish/distributions/nucleus/3.1.1/nucleus-3.1.1.zip";
    // When you use any of the maven urls, add a corresponding dependency on your pom with test scope so that
    // maven will download it into local repo before the test starts.
    private final String MAVEN_GF_311_NUCLEUS_PROFILE = "mvn:org.glassfish.distributions/nucleus/3.1.1/zip";
    private final String MAVEN_GF_311_FULL_PROFILE = "mvn:org.glassfish.distributions/glassfish/3.1.1/zip";
    private final String MAVEN_GF_311_WEB_PROFILE = "mvn:org.glassfish.distributions/web/3.1.1/zip";
    private String gfURL = REMOTE_NUCLEUS_ZIP;

    @Configuration
    public Option[] configure() throws IOException {
        return options(
                mavenBundle().groupId("org.junit").artifactId("com.springsource.org.junit").version("4.8.1"),
                mavenBundle().groupId("org.glassfish.fighterfish").artifactId("sample.embeddedgf.provisioner").version("1.0.0-SNAPSHOT"),
                mavenBundle().groupId("org.glassfish").artifactId("javax.servlet").version("3.1.1"),
                mavenBundle().groupId("org.glassfish.fighterfish").artifactId("test.util").version("1.0.0-SNAPSHOT"),
                systemProperty("org.glassfish.fighterfish.sample.embeddedgf.provisioner.url").value(gfURL),
                systemProperty("pax-exam.framework.shutdown.timeout").value(TIMEOUT.toString())
//                CoreOptions.systemPackages("org.glassfish.embeddable; version=3.1.1", "org.glassfish.embeddable.spi; version=3.1.1"),
        );
    }

    @Test
    public void testSampleProvisioner(BundleContext context) throws Exception {
        GlassFish gf = GlassFishTracker.waitForGfToStart(context, TIMEOUT);
        CommandRunner cr = gf.getCommandRunner();
        cr.setTerse(true);
        CommandResult result = cr.run("version");
        System.out.println(result.getOutput());
        Assert.assertTrue(result.getExitStatus() == CommandResult.ExitStatus.SUCCESS);
    }
}
