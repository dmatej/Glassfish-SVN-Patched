package org.glassfish.fighterfish.sample.testing.failsafe;

import org.glassfish.fighterfish.test.util.FighterFishJUnitRunner;
import org.glassfish.fighterfish.test.util.TestContext;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.osgi.framework.BundleContext;


/**
 * This sample test demonstrates use of a custom JUnit test runner called {@link FighterFishJUnitRunner}
 * to execute a JUnit test using maven surefire plugin. The test name is suffixed with IT so as to be
 * automatically included by maven failsafe plugin.
 *
 * The custom runner has the ability to provision GlassFish, which includes downloading of the GlassFish bundles,
 * installing the smae and bootstrapping GlassFish inside or outside the current JVM. All these steps are
 * pretty configurable via various configuration options specified as system properties. See the pom.xml
 * to see various configuration options. If you chose to control those options from code, then you can provide
 * in a method in test class annotated with @Configuration.
 *
 * @author sanjeeb.sahoo@oracle.com
 */
@RunWith(FighterFishJUnitRunner.class)
public class FighterFishJUnitRunnerIT {
	@Test
	public void test(BundleContext ctx) throws Exception {
		System.out.println("FighterFishJUnitRunnerIT.test()");
		TestContext tc = TestContext.create(getClass());
		try {
            System.out.println("tc.getBundleContext() = " + tc.getBundleContext());
			System.out.println(tc.getGlassFish());
		} finally {
			tc.destroy();
		}
	}

    @Test
    public void test2() throws Exception {
        System.out.println("FighterFishJUnitRunnerIT.test2()");
        TestContext tc = TestContext.create(getClass());
        try {
            System.out.println("tc.getBundleContext() = " + tc.getBundleContext());
            System.out.println(tc.getGlassFish());
        } finally {
            tc.destroy();
        }
    }
}
