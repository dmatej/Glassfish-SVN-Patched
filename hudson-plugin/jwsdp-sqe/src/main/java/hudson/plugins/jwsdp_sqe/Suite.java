package hudson.plugins.jwsdp_sqe;

/**
 * A {@link Suite} is a set of {@link Test}s.
 *
 * @author Kohsuke Kawaguchi
 */
public class Suite extends TestCollection<Suite,Test> {
    public String getChildTitle() {
        return "Test";
    }
}
