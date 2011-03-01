package test;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.XMLStreamWriter2;
import org.codehaus.stax2.XMLStreamProperties;

/**
 * Simple non-automated unit test for outputting namespace-aware XML
 * documents.
 */
public class TestNsStreamWriter3
{
    private TestNsStreamWriter3() {
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
        f.setProperty(XMLStreamProperties.XSP_NAMESPACE_AWARE, Boolean.TRUE);
        f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES,
                      Boolean.TRUE);
        //Boolean.FALSE);

        Writer w = new PrintWriter(System.out);
        XMLStreamWriter2 sw = (XMLStreamWriter2)f.createXMLStreamWriter(w);

        final String URI1 = "http://foo";
        final String URI2 = "http://foo2";
        final String URI3 = "http://foo3";

        sw.writeStartDocument();
        sw.writeStartElement(URI1, "root");
        sw.writeNamespace("foo2", URI2);
        sw.writeDefaultNamespace(URI3);
        sw.writeStartElement(URI3, "leaf");
        sw.writeAttribute(URI2, "ns-attr", "1");
        sw.writeAttribute(null, "ns-attr", "2");
        sw.writeAttribute("otherprefix", URI2, "lastAttr", "x");
        sw.writeEndElement();
        sw.writeEndElement();
        sw.writeCharacters("\n"); // to add lf for terminal output
        sw.writeEndDocument();

        sw.flush();
        sw.close();

        w.close();
    }

    public static void main(String[] args)
        throws Exception
    {
        new TestNsStreamWriter3().test();
    }
}
