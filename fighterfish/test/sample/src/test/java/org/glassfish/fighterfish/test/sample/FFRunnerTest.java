package org.glassfish.fighterfish.test.sample;

import org.glassfish.fighterfish.test.util.TestContext;
import org.glassfish.fighterfish.test.util.FighterFishJUnitRunner;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.osgi.framework.BundleContext;

@RunWith(FighterFishJUnitRunner.class)
public class FFRunnerTest {
	@Test
	public void test(BundleContext ctx) throws Exception {
		System.out.println("GFRunnerTest.test()");
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
        System.out.println("GFRunnerTest.test2()");
        TestContext tc = TestContext.create(getClass());
        try {
            System.out.println("tc.getBundleContext() = " + tc.getBundleContext());
            System.out.println(tc.getGlassFish());
        } finally {
            tc.destroy();
        }
    }
}
