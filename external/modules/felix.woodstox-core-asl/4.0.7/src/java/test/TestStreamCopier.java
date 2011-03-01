package test;

import java.io.*;
import java.util.zip.GZIPInputStream;

import javax.xml.stream.*;

import org.codehaus.stax2.*;
import org.codehaus.stax2.validation.*;

/**
 * Simple test driver for testing pass-through copying using new StAX2
 * methods.
 */
public class TestStreamCopier
{
    final static boolean ENABLE_DTD_VALIDATION = false;
    //final static boolean ENABLE_DTD_VALIDATION = true;

    protected TestStreamCopier() { }

    protected XMLInputFactory2 getFactory()
    {
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");

        XMLInputFactory f =  XMLInputFactory.newInstance();
        //System.out.println("Factory instance: "+f.getClass());

        f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        //f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);
        //f.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, Boolean.FALSE);
        f.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, Boolean.TRUE);

        f.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.TRUE);
        f.setProperty(XMLInputFactory.IS_VALIDATING, Boolean.FALSE);

        f.setProperty(XMLInputFactory2.P_REPORT_PROLOG_WHITESPACE,
                      Boolean.TRUE);

        //f.setProperty(WstxInputProperties.P_MIN_TEXT_SEGMENT, new Integer(9));
        /* Uncomment for boundary-condition stress tests; should be ok to 
         * use some fairly small (but not tiny) number...
         */

        /*
        if (f.isPropertySupported(WstxInputProperties.P_INPUT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_INPUT_BUFFER_LENGTH,
                          new Integer(29));
        }
        if (f.isPropertySupported(WstxInputProperties.P_TEXT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_TEXT_BUFFER_LENGTH,
                          new Integer(20));
        }
        */

        return (XMLInputFactory2) f;
    }

    protected XMLOutputFactory2 getOutputFactory()
    {
        System.setProperty("javax.xml.stream.XMLOutputFactory",
                           "com.ctc.wstx.stax.WstxOutputFactory");

        XMLOutputFactory f =  XMLOutputFactory.newInstance();
        f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES,
                      //Boolean.TRUE
                      Boolean.FALSE
                      );
        return (XMLOutputFactory2) f;
    }

    protected void test(String input, OutputStream out)
        throws Exception
    {
        XMLInputFactory2 ifact = getFactory();
        XMLOutputFactory2 of = getOutputFactory();

        /* Let's have special handling for gzipped stuff...
         */
        XMLStreamReader2 sr;

        if (input.endsWith(".gz")) {
            InputStream in = new GZIPInputStream(new FileInputStream(new File(input)));
            sr = (XMLStreamReader2)ifact.createXMLStreamReader(in);
        } else {
            sr = (XMLStreamReader2)ifact.createXMLStreamReader(new File(input));
        }
        //URL url = new URL("http://www.isb-sib.ch/~ejain/uniprot-rdf/data/taxonomy.rdf.gz");
        //sr = (XMLStreamReader2)ifact.createXMLStreamReader(in);
	
        //XMLStreamWriter2 sw = (XMLStreamWriter2) of.createXMLStreamWriter(out);

        XMLStreamWriter2 sw = (XMLStreamWriter2) of.createXMLStreamWriter(out, "ISO-8859-1");
        //XMLStreamWriter2 sw = (XMLStreamWriter2) of.createXMLStreamWriter(out, "UTF-8");

//System.err.println("[XMLStreamWriter: "+sw.getClass()+"]");

        int count = 0;

        for (int type = sr.getEventType(); type !=XMLStreamConstants.END_DOCUMENT; type = sr.next()) {
            if (type == XMLStreamConstants.DTD) {
                if (ENABLE_DTD_VALIDATION) {
                    DTDInfo info = sr.getDTDInfo();
                    if (info != null) {
                        DTDValidationSchema vld = info.getProcessedDTDSchema();
                        if (vld != null) {
                            System.err.println("Attaching DTD schema: "+vld);
                            sw.validateAgainst(vld);
                        }
                    }
                }
            } else if (type == XMLStreamConstants.COMMENT) {
                //System.err.println("Comment: '"+sr.getText()+"'");
                //if (true) throw new Error("comment");
            }

            if (type == XMLStreamConstants.CHARACTERS) {
                //sw.writeCharacters(sr.getText());
                //sw.writeCharacters(sr.getTextCharacters(), sr.getTextStart(), sr.getTextLength());
                sw.copyEventFromReader(sr, false);
            } else if (type == XMLStreamConstants.CDATA) {
                sw.writeCharacters(sr.getText());
                //sw.writeCharacters(sr.getTextCharacters(), sr.getTextStart(), sr.getTextLength());
            } else if (type == XMLStreamConstants.END_ELEMENT) {
                sw.writeEndElement();
            } else {
                sw.copyEventFromReader(sr, false);
            }

            /*
            if (++count % 1000 == 100) {
                System.err.println("#"+count);
            }
            */
        }

        /*
        do {
            sw.copyEventFromReader(sr, false);
            sr.next();
        } while (sr.hasNext());
        */

        sr.close();
        sw.close();
    }

    public static void main(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+TestStreamCopier.class+" [file]");
            System.exit(1);
        }

        try {
            new TestStreamCopier().test(args[0], System.out);
            System.out.flush();
        } catch (Throwable t) {
          System.err.println("Error: "+t);
          t.printStackTrace();
        }
    }
}
