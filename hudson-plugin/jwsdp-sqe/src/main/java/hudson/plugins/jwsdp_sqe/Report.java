package hudson.plugins.jwsdp_sqe;

import hudson.model.AbstractBuild;
import org.apache.commons.digester.Digester;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

import java.io.File;
import java.io.IOException;

/**
 * Root of the SQE test report.
 *
 * <p>
 * A {@link Report} is a set of {@link Suite}s.
 *
 * @author Kohsuke Kawaguchi
 */
public class Report extends TestCollection<Report,Suite> {
    // parent doesn't exist. so specify dummy

    private final SQETestAction owner;

    public Report(SQETestAction owner) {
        this.owner = owner;
        setName("SQE Test Result");
        setId("root");
    }

    @Override
    public AbstractBuild getOwner() {
        return owner.owner;
    }

    @Override
    public Report getPreviousResult() {
        SQETestAction p = owner.getPreviousResult();
        if(p!=null)
            return p.getResult();
        else
            return null;
    }

    /**
     * Loads SQE report file into this {@link Report} object.
     */
    public void add( File reportXml ) throws IOException, SAXException {
        add(new InputSource(reportXml.toURI().toURL().toExternalForm()));
    }
    
    public void add( InputSource reportXml ) throws IOException, SAXException {
        Digester digester = new Digester();
        digester.setClassLoader(getClass().getClassLoader());

        digester.push(this);

        digester.addObjectCreate("*/testsuite",Suite.class);
        digester.addObjectCreate("*/test",Test.class);
        digester.addObjectCreate("*/testcase",TestCase.class);
        digester.addSetNext("*/testsuite","add");
        digester.addSetNext("*/test","add");
        if(considersTestAsTestObject())
            digester.addCallMethod("*/test", "setconsiderTestAsTestObject");
        digester.addSetNext("*/testcase","add");

        // common properties applicable to more than one TestObjects.
        digester.addBeanPropertySetter("*/id");
        digester.addBeanPropertySetter("*/name");
        digester.addBeanPropertySetter("*/description");
        digester.addSetProperties("*/status","value","statusString");  // set attributes. in particular @revision
        digester.addBeanPropertySetter("*/status","statusMessage");

        digester.parse(reportXml);
    }

    protected boolean considersTestAsTestObject() {
        return owner.considerTestAsTestObject();
    }

    public String getChildTitle() {
        return "Test Suite";
    }
}
