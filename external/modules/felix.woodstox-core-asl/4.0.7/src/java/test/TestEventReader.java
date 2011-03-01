package test;

import java.io.*;
import java.util.Iterator;

import javax.xml.stream.*;
import javax.xml.stream.events.*;

import org.codehaus.stax2.XMLInputFactory2;

import com.ctc.wstx.api.WstxInputProperties;

public class TestEventReader
{
    final XMLInputFactory mFactory;

    public TestEventReader()
    {
        super();
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");
        XMLInputFactory f = XMLInputFactory.newInstance();
        mFactory = f;
        //f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);
        f.setProperty(XMLInputFactory.REPORTER, new TestReporter());

        f.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, Boolean.FALSE);

        // Uncomment for boundary-condition stress tests:
        if (f.isPropertySupported(WstxInputProperties.P_INPUT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_INPUT_BUFFER_LENGTH,
                                 new Integer(32));
        }
        // And let's try to preserve structure as much as possible:
        if (f.isPropertySupported(XMLInputFactory2.P_REPORT_PROLOG_WHITESPACE)) {
            f.setProperty(XMLInputFactory2.P_REPORT_PROLOG_WHITESPACE, Boolean.TRUE);
        }

        if (f.isPropertySupported(WstxInputProperties.P_MIN_TEXT_SEGMENT)) {
            f.setProperty(WstxInputProperties.P_MIN_TEXT_SEGMENT,
                          new Integer(13));
        }

        System.out.println("Factory instance: "+f.getClass());
        System.out.println("  coalescing: "+f.getProperty(XMLInputFactory.IS_COALESCING));
    }

    public void test(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+getClass().getName()+" [file]");
            System.exit(1);
        }
        String filename = args[0];
        File file = new File(filename);
        Reader fin = new java.io.FileReader(file);

        // Let's pass generated system id:
        XMLEventReader er = mFactory.createXMLEventReader(file.toURL().toString(), fin);

        Writer out = new PrintWriter(System.out);

        /*
	{ // Testing peek functionality...
	    XMLEvent e = er.nextTag();
	    out.write("[EVT  "+e.getEventType()+"->");
            e.writeAsEncodedUnicode(out);
	    out.write("]\n");

	    e = er.peek();
	    out.write("[PEEK  "+e.getEventType()+"->");
	    e.writeAsEncodedUnicode(out);
	    out.write("]\n");
	    out.flush();
	}
        */

        //out.write("[START]\n");
        while (er.hasNext()) {
            XMLEvent evt = er.nextEvent();
// Uncomment for debugging:
//System.err.println("["+evt.getEventType()+"]: '");
            if (evt.isStartElement()) {
                StartElement elem = (StartElement) evt;
                Iterator it = elem.getNamespaces();
                int count = 0;
                while (it.hasNext()) {
                    it.next();
                    ++count;
                }
//System.err.println("[Ns count: "+count+"]");
            } else if (evt.isCharacters()) {
                Characters chars = evt.asCharacters();
                int len = chars.getData().length();
                out.write("[CHARACTERS("+len+"), ws: "+chars.isWhiteSpace()+", iws: "+chars.isIgnorableWhiteSpace()+"]");
            } else if (evt instanceof EntityReference) {
		EntityReference eref = (EntityReference) evt;
                out.write("[ENTITY-REF '"+eref.getName()+"']");
            }
            //out.write("{ENC:");
            evt.writeAsEncodedUnicode(out);
            //out.write("}");
            //out.write("'\n");
            //out.write('\n');
            //out.flush();
        }
        //out.write("[END]\n");
        out.flush();
    }

    public static void main(String[] args) throws Exception
    {
        // Uncomment for infinite looping (stress test)

        /*
        int count = 0;
        while (true) {
            ++count;
            System.err.println("#"+count);
        */
            new TestEventReader().test(args);
            /*
        }
            */
    }
}
