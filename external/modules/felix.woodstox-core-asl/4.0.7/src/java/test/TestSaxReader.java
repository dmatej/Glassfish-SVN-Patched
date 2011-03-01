package test;

import java.io.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.*;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.DefaultHandler2;

import com.ctc.wstx.sax.*;

/**
 * Simple helper test class for checking how the parser works when
 * used via SAX interface.
 */
public class TestSaxReader
{
    protected TestSaxReader() {
    }

    protected void test(File file)
        throws Exception
    {
        //SAXParserFactory spf = SAXParserFactory.newInstance();
        SAXParserFactory spf = new WstxSAXParserFactory();
        spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
        SAXParser sp = spf.newSAXParser();
        MyContentHandler h = new MyContentHandler();
        sp.setProperty(SAXProperty.LEXICAL_HANDLER.toString(), (DeclHandler) h);
        // This shouldn't be needed -- nor seems to work for Xerces?
        //sp.getXMLReader().setEntityResolver(h);
        InputStream in = new FileInputStream(file);
        InputSource src = new InputSource(in);
        src.setSystemId(file.toURL().toExternalForm());
        sp.parse(src, h);
    }

    /*
    ////////////////////////////////////////////////
    // Helper class
    ////////////////////////////////////////////////
     */

    final static class MyContentHandler
        extends DefaultHandler2
        implements DeclHandler
    {
        Locator mLocator;

        public MyContentHandler() { }

        public void characters(char[] ch, int start, int length)
        {
            System.out.print("[CHARACTERS] (len "+length+"): '");
            printString(ch, start, length);
            System.out.println("'");
        }

        public void endDocument()
        {
            System.out.println("[END-DOCUMENT]");
        }

        public void endElement(String nsUri, String localName, String qName)
        {
            System.out.print("[END-ELEMENT] </");
            System.out.print(getName(qName, nsUri));
            System.out.println(">");
        }

        public void endPrefixMapping(String prefix)
        {
            System.out.println("[UNMAP-PREFIX '"+prefix+"']");
        }

        public void ignorableWhitespace(char[] ch, int start, int length)
        {
            System.out.println("[IGN-WS] (len "+length+")");
        }

        public void processingInstruction(String target, String data)
        {
            System.out.println("[PROC-INSTR '"+target+"' ...]");
        }

        public void setDocumentLocator(Locator locator)
        {
            mLocator = locator;
        }

        public void skippedEntity(String name)
        {
            System.out.println("[SKIPPED-entity '"+name+"']");
        }

        public void startDocument()
        {
            System.out.println("[START-DOC]");
        }

        public void startElement(String nsUri, String localName, String qName, Attributes attrs)
        {
            System.out.print("[START-ELEMENT] (");
            System.out.print(attrs.getLength());
            System.out.print(" attrs) <");
            System.out.print(getName(qName, nsUri));

            for (int i = 0, len = attrs.getLength(); i < len; ++i) {
                System.out.print(' ');
                System.out.print(getName(attrs.getQName(i), attrs.getURI(i)));
                System.out.print("='");
                System.out.print(attrs.getValue(i));
                System.out.print("'");
            }
            System.out.println(">");
            System.out.println(" (Location, system id = '"+mLocator.getSystemId()+"')");
        }

        public void startPrefixMapping(String prefix, String uri)
        {
            System.out.println("[MAP-PREFIX '"+prefix+"'->'"+uri+"']");
        }

        public void notationDecl(String name, String publicId, String systemId)
        {
            System.out.println("[NOTATION-DECL pub='"+publicId+"' sys='"+systemId+"']");
        }

        public void unparsedEntityDecl(String name, String publicId, String systemId, String notationName)
        {
            System.out.println("[UNPARSED-ENTITY-DECL '"+name+"']");
        }

        public void warning(SAXParseException e)
        {
            System.out.println("[WARNING: '"+e.getMessage()+"']");
        }

        // // // LexicalHandler:

        public void comment(char[] ch, int start, int length)
        {
            System.out.print("[COMMENT] '");
            printString(ch, start, length);
            System.out.println("'");
        }

        public void endCDATA()
        {
            System.out.println("[END-CDATA]");
        }

        public void endDTD()
        {
            System.out.println("[END-DTD]");
        }

        public void endEntity(String name)
        {
            System.out.println("[END-ENTITY '"+name+"']");
        }

        public void startCDATA()
        {
            System.out.println("[START-CDATA]");
        }

        public void startDTD(String name, String publicId, String systemId)
        {
            System.out.print("[START-DTD ");
            System.out.print(name);
            System.out.println("]");
        }

        public void startEntity(String name) 
        {
            System.out.println("[START-ENTITY '"+name+"']");
        }

        /* Note: DefaultHandler also implements EntityResolver; and
         * SAXParser sets it as the default entity resolver when
         * its parse() method gets called.
         */
        public InputSource resolveEntity(String publicId, String systemId) 
        {
            System.out.println("[RESOLVE-ENTITY pub '"+publicId+"' sys '"+systemId+"']");
            return null;
        }

        private void printString(char[] ch, int start, int length)
        {
            if (length < 60) {
                System.out.print(new String(ch, start, length));
            } else {
                StringBuffer sb = new StringBuffer(64);
                sb.append(ch, start, 28);
                sb.append("]..[");
                sb.append(ch, (start + length - 28), 28);
            }
        }

        private String getName(String qname, String uri) {
            if (uri == null) {
                return qname;
            }
            return qname + " {" + uri + "}";
        }
    }

    /*
    ////////////////////////////////////////////////
    // Main method
    ////////////////////////////////////////////////
     */

    public static void main(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+TestSaxReader.class+" [file]");
            System.exit(1);
        }

        try {
            new TestSaxReader().test(new File(args[0]));
            System.out.println("\nDone!");
        } catch (Throwable t) {
            System.err.println("Error: "+t);
            t.printStackTrace();
        }
    }
}
