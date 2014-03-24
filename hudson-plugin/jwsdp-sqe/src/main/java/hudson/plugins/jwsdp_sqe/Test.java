package hudson.plugins.jwsdp_sqe;

/**
 * A {@link Test} is a set of {@link TestCase}s.
 *
 * <p>
 * And it's apparently also counted as a runnable test by itself.
 * Ugly design, if you ask me.
 *
 * @author Kohsuke Kawaguchi
 */
public class Test extends TestCollection<Test,TestCase> {
    boolean considerTestAsTestObject = false;
    public String getChildTitle() {
        return "Test Case";
    }

    public int getTotalCount() {
        if(considerTestAsTestObject)
            return super.getTotalCount()+1;
        else {
            if(super.getTotalCount() != 0)
                return super.getTotalCount();
            else
                return 1;
        }
    }

    public int getFailCount() {
        if(considerTestAsTestObject)
            return super.getFailCount() + (getStatus()==Status.PASS ? 0 : 1);
        else {
            if (super.getTotalCount() != 0)
                return super.getFailCount();
            else
                return (getStatus() == Status.PASS ? 0 : 1);
        }
    }

    public void setconsiderTestAsTestObject() {
        this.considerTestAsTestObject = true;
    }
}
