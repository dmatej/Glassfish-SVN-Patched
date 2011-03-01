package test;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.XMLOutputFactory2;
import org.codehaus.stax2.XMLStreamProperties;

import com.ctc.wstx.api.InvalidCharHandler;
import com.ctc.wstx.api.WstxOutputProperties;

/**
 * Simple non-automated unit test for outputting namespace-aware XML
 * documents.
 */
public class TestNsStreamWriter
{
    private TestNsStreamWriter() {
    }

    protected XMLOutputFactory getFactory()
    {
        System.setProperty("javax.xml.stream.XMLOutputFactory",
                           "com.ctc.wstx.stax.WstxOutputFactory");
        return XMLOutputFactory.newInstance();
    }

    //final String ENCODING = "ISO-8859-1";
    final String ENCODING = "UTF-8";

    protected void test()
        throws Exception
    {
        XMLOutputFactory f = getFactory();
        f.setProperty(XMLStreamProperties.XSP_NAMESPACE_AWARE,
                      Boolean.TRUE);
        //Boolean.FALSE);
        f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES,
                      //Boolean.TRUE);
                      Boolean.FALSE);
        f.setProperty(XMLOutputFactory2.P_AUTOMATIC_EMPTY_ELEMENTS,
                      //Boolean.TRUE);
                      Boolean.FALSE);

        f.setProperty(WstxOutputProperties.P_OUTPUT_VALIDATE_CONTENT,
                      Boolean.TRUE);
        f.setProperty(WstxOutputProperties.P_OUTPUT_FIX_CONTENT,
                      Boolean.TRUE);
                      //Boolean.FALSE);

        /* 11-Nov-2008, TSa: Let's try out this new property, created
         *   for [WSTX-167]:
         */
        //f.setProperty(WstxOutputProperties.P_OUTPUT_INVALID_CHAR_HANDLER, new InvalidCharHandler.ReplacingHandler('X'));

        //Writer w = new PrintWriter(System.out);
        //XMLStreamWriter sw = f.createXMLStreamWriter(w);

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        XMLStreamWriter sw = f.createXMLStreamWriter(bos, ENCODING);
        //XMLStreamWriter sw = f.createXMLStreamWriter(bos);

        sw.writeStartDocument();

        {
            final String TEXT =
                "Comment, invalid: \u0003"
                ;
            sw.writeComment(TEXT);
        }

        sw.writeCharacters("\n");
        sw.writeStartElement("root");

        sw.setPrefix("ns", "http://foo");

        System.out.println("Prefix 'ns' -> ["+sw.getPrefix("ns")+"]");

        //sw.writeAttribute("attr", "Invalid also: \0");

        sw.writeCharacters("Need to quote this too: ]]> plus invalid... {\u0012}");

        /*
        sw.writeEmptyElement("alpha");
        sw.writeNamespace("ns", "uri:foo");
        sw.writeAttribute("atpr", "http://attr-prefix", "attr", "a<b");

        sw.writeStartElement("bravo");

        sw.writeCharacters("Text: & \n");
        */

        sw.writeCData("Test: ]]>x");
        sw.writeProcessingInstruction("p", "i");

        sw.writeEndElement(); // exception here

        sw.writeCharacters("\n"); // to get linefeed
        sw.writeEndDocument();

        sw.flush();
        sw.close();

        //w.close();

        System.err.println("DOC -> '"+new String(bos.toByteArray(), ENCODING)+"'");
    }

    public static void main(String[] args)
        throws Throwable
    {
        try {
            new TestNsStreamWriter().test();
        } catch (XMLStreamException ex) {
            if (ex.getCause() != null) {
                throw ex.getCause();
            }
            throw ex;
        }
    }
}
