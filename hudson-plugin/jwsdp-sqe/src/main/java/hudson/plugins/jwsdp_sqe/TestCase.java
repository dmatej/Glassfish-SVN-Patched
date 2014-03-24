package hudson.plugins.jwsdp_sqe;

/**
 * Represents one test case.
 *
 * @author Kohsuke Kawaguchi
 */
public class TestCase extends TestObject<TestCase> {
    public int getTotalCount() {
        return 1;
    }

    public int getFailCount() {
        return getStatus()==Status.PASS ? 0 : 1;
    }
}
