package hudson.plugins.jwsdp_sqe;

import hudson.model.Build;
import hudson.model.BuildListener;
import hudson.model.AbstractBuild;
import hudson.tasks.junit.TestResult;
import hudson.tasks.test.AbstractTestResultAction;
import org.kohsuke.stapler.StaplerProxy;
import org.xml.sax.SAXException;

import java.io.File;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * {@link hudson.model.Action} that displays the SQE test result.
 *
 * <p>
 * The actual test reports are isolated by {@link WeakReference}
 * so that it doesn't eat up too much memory.
 *
 * @author Kohsuke Kawaguchi
 */
public class SQETestAction extends AbstractTestResultAction<SQETestAction> implements StaplerProxy {
    private transient WeakReference<Report> result;
    private boolean considerTestAsTestObject = false;
    private int failCount;
    private int totalCount;

    SQETestAction(Build owner, BuildListener listener, boolean considerTestAsTestObject) {
        super(owner);
        this.considerTestAsTestObject = considerTestAsTestObject;

        Report r = load(listener);
        totalCount = r.getTotalCount();
        failCount = r.getFailCount();

        result = new WeakReference<Report>(r);
    }

    /*package*/ static File getDataDir(AbstractBuild build) {
        return new File(build.getRootDir(), "sun-sqe-result");
    }

    public boolean considerTestAsTestObject() {
        return considerTestAsTestObject;
    }
    public synchronized Report getResult() {
        if(result==null) {
            Report r = load(null);
            result = new WeakReference<Report>(r);
            return r;
        }
        Report r = result.get();
        if(r==null) {
            r = load(null);
            result = new WeakReference<Report>(r);
        }
        return r;
    }

    /**
     * Gets the number of failed tests.
     */
    public int getFailCount() {
        return failCount;
    }

    /**
     * Gets the total number of tests.
     */
    public int getTotalCount() {
        return totalCount;
    }

    /**
     * Loads a {@link TestResult} from disk.
     *
     * @param listener
     *      Can be null. If available, error reports should be sent there.
     */
    private Report load(BuildListener listener) {
        Report r = new Report(this);
        File[] files = getDataDir(owner).listFiles();
        if(files==null) {
            logger.log(Level.WARNING, "No test reports found in "+getDataDir(owner));
            return r;
        }

        for (File f : files) {
            try {
                if(f.isFile() && f.getName().endsWith(".xml"))
                    r.add(f);
            } catch (IOException e) {
                if(listener!=null)
                    e.printStackTrace(listener.error("Failed to parse "+f));
                else
                    logger.log(Level.WARNING, "Failed to load "+f,e);
            } catch (SAXException e) {
                if(listener!=null)
                    e.printStackTrace(listener.error("Failed to parse "+f));
                else
                    logger.log(Level.WARNING, "Failed to load "+f,e);
            }
        }

        return r;
    }

    public Object getTarget() {
        return getResult();
    }

    private static final Logger logger = Logger.getLogger(SQETestAction.class.getName());
}
