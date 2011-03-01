package test;

import java.io.*;

import javax.xml.parsers.*;
import javax.xml.transform.Source;
import javax.xml.transform.dom.*;
import javax.xml.stream.*;

import org.w3c.dom.*;
import org.xml.sax.InputSource;

import org.codehaus.stax2.*;

public class TestDomReader
{
    public static void main(String[] args)
        throws Exception
    {
        String XML = "<blah xmlns=\"http://blah.org\"><foo>foo</foo></blah>";
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(false);
        dbf.setValidating(false);
        DocumentBuilder builder = dbf.newDocumentBuilder();
        Document doc = builder.parse(new InputSource(new StringReader(XML)));

        // to see if Xerces (etc) mind illegal chars, try:
        /*
        Text txt = doc.createTextNode("Invalid: \u000C./"+((char) 0)+"..");
        doc.getDocumentElement().appendChild(txt);
        */

        Source source = new DOMSource(doc);
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");

        XMLInputFactory f = XMLInputFactory.newInstance();
        //f.setProperty(XMLInputFactory2.P_INTERN_NAMES, Boolean.TRUE);
        System.out.println("DEBUG: factory, intern names = "+f.getProperty(XMLInputFactory2.P_INTERN_NAMES));

        XMLStreamReader sr = f.createXMLStreamReader(source);
        System.out.println("DEBUG: reader, intern names = "+sr.getProperty(XMLInputFactory2.P_INTERN_NAMES));

        while (sr.hasNext()) {
            int type = sr.next();
            System.out.print("["+type+"]");
            
            if (sr.hasName()) {
                System.out.println(" name = '"+sr.getName()+"'");
            }
            
            System.out.println();
        }
    }
}


