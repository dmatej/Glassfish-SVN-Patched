package wstxtest.sax;

import java.io.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;

import com.ctc.wstx.sax.*;

import wstxtest.BaseWstxTest;

/**
 * Simple unit tests to verify that most fundamental parsing functionality
 * works via Woodstox SAX implementation.
 */
public class TestBasicSax
    extends BaseWstxTest
{
    final static String XML = "<?xml version='1.0'?>\n"
        +"<!DOCTYPE root []>"
        +"<root><!-- comment -->"
        +"<leaf attr='a&amp;b!'>rock&apos;n <![CDATA[roll]]></leaf><?proc instr?></root>  ";

    public void testSimpleNs()
        throws Exception
    {
        doTestSimple(true, false);
        doTestSimple(true, true);
    }

    public void testSimpleNonNs()
        throws Exception
    {
        doTestSimple(false, false);
        doTestSimple(false, true);
    }

    /*
    ////////////////////////////////////////////////////
    // Helper methods
    ////////////////////////////////////////////////////
     */

    public void doTestSimple(boolean ns, boolean useReader)
        throws Exception
    {
        // no need to test JAXP introspection...
        SAXParserFactory spf = new WstxSAXParserFactory();
        spf.setNamespaceAware(ns);
        SAXParser sp = spf.newSAXParser();
        MyHandler h = new MyHandler();

        InputSource src;

        if (useReader) {
            src = new InputSource(new StringReader(XML));
        } else {
            src = new InputSource(new ByteArrayInputStream(XML.getBytes("UTF-8")));
        }

        sp.parse(src, h);
        assertEquals(2, h._elems);
        assertEquals(1, h._attrs);
        assertEquals(11, h._charCount);
    }

    final static class MyHandler
        extends DefaultHandler
    {
        public int _elems, _attrs;

        public int _charCount;

        public void characters(char[] ch, int start, int length) {
            _charCount += length;
        }
     
        public void startElement(String uri, String ln, String qname,
                                 Attributes a)
        {
            ++_elems;
            int ac = a.getLength();
            _attrs += ac;

            for (int i = 0; i < ac; ++i) {
                a.getLocalName(i);
                a.getQName(i);
                a.getURI(i);
                a.getValue(i);
                a.getType(i);
            }
        }
    }
}
