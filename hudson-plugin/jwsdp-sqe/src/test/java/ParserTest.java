import hudson.plugins.jwsdp_sqe.Report;
import junit.framework.TestCase;
import org.xml.sax.InputSource;

/**
 * @author Kohsuke Kawaguchi
 */
public class ParserTest extends TestCase {
//    public void test1() throws Exception {
//        parse("test.xml");
//    }

    public void test2() throws Exception {
        parse("resultValid.xml");
    }
    
    public void test3() throws Exception {
        parse("jaxb-sqeValid.xml");
    }

    private void parse(String res) throws Exception {
        Report r = new Report(null) {
            @Override
            protected boolean considersTestAsTestObject() {
                return false;
            }
        };
        r.add(new InputSource(getClass().getResource(res).toExternalForm()));
        System.out.println(r.getFailCount()+"/"+r.getTotalCount());
    }
}
