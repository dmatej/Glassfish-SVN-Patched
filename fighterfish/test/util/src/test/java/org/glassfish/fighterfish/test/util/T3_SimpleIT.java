/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012-2013 Oracle and/or its affiliates. All rights reserved.
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

import org.glassfish.embeddable.GlassFishException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ops4j.pax.exam.Option;
import org.ops4j.pax.exam.junit.Configuration;
import org.ops4j.pax.exam.junit.ExamReactorStrategy;
import org.ops4j.pax.exam.junit.JUnit4TestRunner;
import org.ops4j.pax.exam.spi.reactors.EagerSingleStagedReactorFactory;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import javax.inject.Inject;
import java.io.IOException;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * @author sanjeeb.sahoo@oracle.com
 */
@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
public class T3_SimpleIT {

    /**
     * Pax-Exam supports injection.
     */
    @Inject
    private BundleContext ctx;

    /**
     * PaxExamJunit driver treats methods in Junit Test class annotated with @Configuration specially.
     * For each such method, it creates a separate test container configuring it with the options as returned
     * by the method.
     *
     * @return Options used to configure a test container
     * @throws IOException
     */
    @Configuration
    public Option[] getPaxExamConfiguration() throws IOException {
        return TestsConfiguration.getInstance().getPaxExamConfiguration();
    }

    /**
     * A simple test case that deploys a couple of bundles one of which is an API bundle
     * which is consumed by the second one which is a WAB. It then requests a resource
     * from the WAB and compares the output with an expected output.
     *
     * The test uses mvn url scheme to reference the source location of bundles to be deployed.
     * You must have the maven artifacts available in your local or remote maven repo.
     *
     * The test will automatically provision a GlassFish runtime for you.
     *
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     * @throws IOException
     */
    @Test
    public void test() throws GlassFishException, InterruptedException, BundleException, IOException {
        assertNotNull(ctx);
        TestContext tc = TestContext.create(getClass());
        try {
        	// Let's install a couple of bundles one of which is an API bundle
        	// which is consumed by the second one which is a WAB.
            Bundle uas_api_b = tc.installBundle("mvn:org.glassfish.fighterfish/sample.uas.api/1.0.0");
            Bundle uas_simplewab_b = tc.installBundle("mvn:org.glassfish.fighterfish/sample.uas.simplewab/1.0.0/war");
            WebAppBundle uas_simple_webapp = tc.deployWebAppBundle(uas_simplewab_b);
            String response = uas_simple_webapp.getHttpGetResponse("/LoginServlet?name=foo&password=bar");
            System.out.println(response);
            assertThat(response, new StringPatternMatcher("Service is not yet available"));
        } finally {
            tc.destroy();
        }
    }

}
