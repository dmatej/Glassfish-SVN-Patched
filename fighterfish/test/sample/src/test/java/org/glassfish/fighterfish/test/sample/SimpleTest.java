package org.glassfish.fighterfish.test.sample;

import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.glassfish.embeddable.GlassFishException;
import org.glassfish.fighterfish.test.util.StringPatternMatcher;
import org.glassfish.fighterfish.test.util.TestContext;
import org.glassfish.fighterfish.test.util.TestsConfiguration;
import org.glassfish.fighterfish.test.util.WebAppBundle;
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

@RunWith(JUnit4TestRunner.class)
@ExamReactorStrategy( EagerSingleStagedReactorFactory.class )
public class SimpleTest {
	
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
     * @param ctx
     * @throws GlassFishException
     * @throws InterruptedException
     * @throws BundleException
     * @throws IOException
     */
    @Test
    public void test(BundleContext ctx) throws GlassFishException, InterruptedException, BundleException, IOException {
        TestContext tc = TestContext.create(ctx);
        try {
        	// Let's install a couple of bundles one of which is an API bundle 
        	// which is consumed by the second one which is a WAB. 
            Bundle uas_api_b = tc.installBundle("mvn:org.glassfish.fighterfish/sample.uas.api/1.0.0-SNAPSHOT");
            Bundle uas_simplewab_b = tc.installBundle("mvn:org.glassfish.fighterfish/sample.uas.simplewab/1.0.0-SNAPSHOT/war");
            WebAppBundle uas_simple_webapp = tc.deployWebAppBundle(uas_simplewab_b);
            String response = uas_simple_webapp.getResponse("/LoginServlet?name=foo&password=bar");
            System.out.println(response);
            assertThat(response, new StringPatternMatcher("Service is not yet available"));
        } finally {
            tc.destroy();
        }
    }

}
