package test;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.XMLInputFactory2;

import com.ctc.wstx.api.WstxInputProperties;

/**
 * Simple non-automated unit test for running validating stream reader on
 * given document.
 */
public class TestValidation
    extends TestStreamReader
{
    private TestValidation() {
    }

    protected XMLInputFactory2 getFactory()
    {
        XMLInputFactory2 f = super.getFactory();

        f.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.TRUE);
        f.setProperty(XMLInputFactory.IS_VALIDATING, Boolean.TRUE);

        // Usually we don't care about full stress testing...
        if (f.isPropertySupported(WstxInputProperties.P_INPUT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_INPUT_BUFFER_LENGTH,
                          new Integer(2000));
        }

        // Do we want to handle validation problems gracefully?
        //f.setProperty(XMLInputFactory.REPORTER, new TestReporter());

        return f;
    }

    public static void main(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+TestValidation.class+" [file]");
            System.exit(1);
        }
        try {
            int total = new TestValidation().test(new File(args[0]));
            System.out.println("Total: "+total);
        } catch (XMLStreamException sex) {
            System.err.println("XML Error: "+sex);
        } catch (Throwable t) {
            System.err.println("Error: "+t);
            t.printStackTrace();
        }
    }
}
