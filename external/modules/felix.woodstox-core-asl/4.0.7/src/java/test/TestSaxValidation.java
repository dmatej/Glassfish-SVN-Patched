package test;

import java.io.*;
import javax.xml.parsers.*; // TRAX, for creating parsers

import org.xml.sax.*;

/**
 * Simple test class used to check how other validation xml parsers (like
 * Xerces) deal with dtd (etc) validation issues
 */
public class TestSaxValidation
{
    private TestSaxValidation() { }

    protected int test(File f)
        throws Exception
    {
        /*
        System.setProperty("javax.xml.parsers.SAXParserFactory",
                           "org.apache.xerces.jaxp.SAXParserFactoryImpl");
        */
        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(true);
        System.out.println("SAX factory: "+factory.getClass());
        SAXParser parser = factory.newSAXParser();
        XMLReader xr = parser.getXMLReader();

        xr.setContentHandler(new MyHandler());
        xr.parse(new InputSource(new FileInputStream(f)));

        return 1;
   }

    public static void main(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+TestSaxValidation.class+" [file]");
            System.exit(1);
        }
        try {
          int total = new TestSaxValidation().test(new File(args[0]));
          System.out.println("Total: "+total);
        } catch (Throwable t) {
          System.err.println("Error: "+t);
          t.printStackTrace();
        }
    }

    final static class MyHandler
        extends org.xml.sax.helpers.DefaultHandler
    {
        public void startElement(String uri, String ln, String qname,
                                 Attributes attrs)
        {
            System.err.println("Elem <"+qname+">, "+attrs.getLength()+" attrs.");
        }
    }
}
