package test;

import java.io.*;

import javax.xml.stream.*;
import javax.xml.transform.stream.StreamResult;

import org.codehaus.stax2.XMLOutputFactory2;
import org.codehaus.stax2.XMLStreamProperties;
import org.codehaus.stax2.XMLStreamWriter2;
import org.codehaus.stax2.validation.*;

import com.ctc.wstx.api.WstxOutputProperties;

/**
 * Simple non-automated unit test for outputting non-namespace-aware XML
 * documents.
 */
public class TestNonNsStreamWriter
{
    private TestNonNsStreamWriter() {
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
        f.setProperty(XMLStreamProperties.XSP_NAMESPACE_AWARE,
                      Boolean.FALSE);
        f.setProperty(XMLOutputFactory2.P_AUTOMATIC_EMPTY_ELEMENTS,
                      Boolean.TRUE);
        f.setProperty(WstxOutputProperties.P_OUTPUT_VALIDATE_NAMES,
                      Boolean.TRUE);

        StringWriter w = new StringWriter()
            /*
              {
                public void close() {
                    if (true) throw new Error();
                }
            }
            */
            ;

        //XMLStreamWriter2 sw = (XMLStreamWriter2) f.createXMLStreamWriter(w);
        XMLStreamWriter2 sw = (XMLStreamWriter2) f.createXMLStreamWriter(new StreamResult(w));

        final String dtdStr =
            "<!ELEMENT root (elem, elem3)>\n"
            +"<!ATTLIST root attr CDATA #IMPLIED>\n"
            +"<!ATTLIST root another CDATA #IMPLIED>\n"
            +"<!ELEMENT elem ANY>\n"
            +"<!ELEMENT elem3 ANY>\n"
            ;

        XMLValidationSchemaFactory vd = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_DTD);

        XMLValidationSchema schema = vd.createSchema(new StringReader(dtdStr));

        //sw.validateAgainst(schema);

        sw.writeStartDocument();
        sw.writeComment("Comment!");
        sw.writeCharacters("\r");
        sw.writeStartElement("root");
        sw.writeAttribute("attr", "value");
        sw.writeAttribute("another", "this & that");
        //sw.writeAttribute("attr", "whatever"); // error!
        sw.writeStartElement(null, "elem");
        sw.writeCharacters("Sub-text");
        sw.writeEndElement();
        //sw.writeStartElement("elem3:foo"); // error, colon inside local name
        sw.writeStartElement("elem3");
        sw.writeEndElement();
        //sw.writeCharacters("Root text <> ]]>\n");
        sw.writeEndElement();
        //sw.writeEmptyElement("secondRoot"); // error!
        sw.writeCharacters("\n"); // white space in epilog
        sw.writeProcessingInstruction("target", "some data");
        sw.writeCharacters("\n"); // white space in epilog
        sw.writeEndDocument();

        sw.flush();
        sw.close();

        System.out.println("DOC = ["+w.toString()+"]");
        //System.out.println("sw = "+sw);
    }

    public static void main(String[] args)
        throws Exception
    {
        new TestNonNsStreamWriter().test();
    }
}
