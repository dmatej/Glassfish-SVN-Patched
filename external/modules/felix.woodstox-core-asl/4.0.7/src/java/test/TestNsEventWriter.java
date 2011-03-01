package test;

import java.io.*;
import java.util.*;

import javax.xml.namespace.QName;
import javax.xml.stream.*;

import org.codehaus.stax2.XMLOutputFactory2;
import org.codehaus.stax2.XMLStreamProperties;

import com.ctc.wstx.stax.WstxEventFactory;

/**
 * Simple non-automated unit test for outputting namespace-aware XML
 * documents.
 */
public class TestNsEventWriter
{
    private TestNsEventWriter() {
    }

    protected XMLOutputFactory getFactory()
    {
        System.setProperty("javax.xml.stream.XMLOutputFactory",
                           "com.ctc.wstx.stax.WstxOutputFactory");
        return XMLOutputFactory.newInstance();
    }

    protected void test()
        throws Exception
    {
        XMLOutputFactory f = getFactory();
        f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES,
                      Boolean.TRUE);
        f.setProperty(XMLStreamProperties.XSP_NAMESPACE_AWARE,
                      Boolean.TRUE);
        f.setProperty(XMLOutputFactory2.P_AUTOMATIC_EMPTY_ELEMENTS,
                      Boolean.TRUE);
        Writer w = new PrintWriter(System.out);
        XMLEventWriter ew = f.createXMLEventWriter(w);
        XMLEventFactory evtF = new WstxEventFactory();

        ew.add(evtF.createStartDocument("UTF-8", "1.1", true));
        ew.add(evtF.createComment("Comment!"));
        ew.add(evtF.createCharacters("\n"));
        ew.add(evtF.createStartElement(new QName("http://mydomain", "root"),
                                       null, null));

        // Need to first create ns & attrs for next element:
        ArrayList attrs = new ArrayList();
        attrs.add(evtF.createAttribute(new QName("attr"), "value"));
        attrs.add(evtF.createAttribute(new QName("http://attr-prefix", "aptr", "attr"), "value"));
        attrs.add(evtF.createAttribute(new QName("http://attr-prefix", "attr3"), "value!"));
        attrs.add(evtF.createAttribute(new QName("another"), "this & that"));
        //attrs.add(evtF.createAttribute(new QName("attr"), "whatever"); // error

        ArrayList ns = new ArrayList();
        ns.add(evtF.createNamespace("http://default")); // error if not output
        ns.add(evtF.createNamespace("myprefix", "http://mydotcom")); // - "" -

        ew.add(evtF.createStartElement(new QName("elem"),
               attrs.iterator(), ns.iterator()));

        ew.add(evtF.createCharacters("Sub-text"));
        ew.add(evtF.createEndElement(new QName("elem"), null));
        //ew.add(evtF.createEndElement(new QName("elem3:foo"), null));

        ew.add(evtF.createStartElement(new QName("elem3"), null, null));
        ew.add(evtF.createEndElement(new QName("elem3"), null));


        // Let's create more namespaces...
        ew.add(evtF.createStartElement(new QName("http://attr-prefix", "branch"), null, null));

        // Comment out to test that close() works...
        /*
        ew.add(evtF.createCharacters("Root text <> ]]>\n"));
        ew.add(evtF.createEndElement(new QName("root"), null));
        //ew.add(evtF.createEndElement(new QName("secondRoot"), null)); // error!
        ew.add(evtF.createCharacters("\n")); // white space in epilog
        ew.add(evtF.createProcessingInstruction("target", "some data"));
        ew.add(evtF.createCharacters("\n"));
        ew.add(evtF.createEndDocument());
        */

        ew.flush();
        ew.close();

        w.close();
    }

    public static void main(String[] args)
        throws Exception
    {
        new TestNsEventWriter().test();
    }
}
