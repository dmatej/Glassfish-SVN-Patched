package test;

import java.io.*;

import javax.xml.XMLConstants;
import javax.xml.stream.*;

import org.codehaus.stax2.XMLOutputFactory2;
import org.codehaus.stax2.XMLStreamProperties;

/**
 * Simple non-automated unit test for outputting namespace-aware XML
 * documents.
 */
public class TestNsStreamWriter2
{
    private TestNsStreamWriter2() {
    }

    protected XMLOutputFactory getFactory()
    {
        System.setProperty("javax.xml.stream.XMLOutputFactory",
                           "com.ctc.wstx.stax.WstxOutputFactory");
        return XMLOutputFactory.newInstance();
    }

    private String namespace = "http://www.w3.org/2003/05/soap-envelope";
    private String TS_NS = "http://tatu.org";

    protected void test()
        throws Exception
    {
        XMLOutputFactory f = getFactory();
        f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES,
                      //Boolean.FALSE);
                      Boolean.TRUE);
        f.setProperty(XMLStreamProperties.XSP_NAMESPACE_AWARE,
                      Boolean.TRUE);
        f.setProperty(XMLOutputFactory2.P_AUTOMATIC_EMPTY_ELEMENTS,
                      Boolean.TRUE);
        Writer w = new PrintWriter(System.out);
        XMLStreamWriter sw = f.createXMLStreamWriter(w);

        sw.writeStartDocument();
        sw.setPrefix("env", namespace);
        //sw.setPrefix("test", "http://someTestUri");

        //sw.writeStartElement("env", "Envelope", namespace);
        // or: 
        sw.writeStartElement(namespace, "Envelope");

        //sw.writeNamespace("env", namespace);
        //sw.writeNamespace("test", "http://someTestUri");

        sw.writeEmptyElement("xml", "stdTag", XMLConstants.XML_NS_URI);
        sw.writeAttribute("xml", XMLConstants.XML_NS_URI, "lang", "fi-FI");
        sw.setDefaultNamespace(TS_NS);
        sw.writeEmptyElement(TS_NS, "elem");
        sw.writeAttribute(TS_NS, "attr", "value");
        // Let's also ensure that we will reuse that ns...
        sw.writeAttribute(TS_NS, "attr2", "value2");
        // Should NOT use the suggested prefix (already taken)
        sw.writeAttribute("wstxns1", "http://foo", "attr3", "value3");
        sw.writeAttribute("env2", "http://foo2", "attr4", "value4");

        sw.writeCharacters("\n");
        sw.writeEndElement();

        /*
        sw.writeComment("end");
        */

        // Let's add an LF; easier to display output to console
        sw.writeCharacters("\n");
        sw.writeEndDocument();

        sw.flush();
        sw.close();

        w.close();
    }

    public static void main(String[] args)
        throws Exception
    {
        new TestNsStreamWriter2().test();
    }
}
