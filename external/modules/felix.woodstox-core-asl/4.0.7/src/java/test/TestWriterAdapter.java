package test;

import java.io.*;

import javax.xml.namespace.QName;
import javax.xml.stream.*;

import org.codehaus.stax2.*;
import org.codehaus.stax2.ri.Stax2WriterAdapter;

/**
 * Simple test class used for manual verification of Stax2WriterAdapter
 * features.
 */
public class TestWriterAdapter
{
    // Need a dummy base class to access protected constructor for testing
    final static class MyAdapter
        extends Stax2WriterAdapter
    {
        public MyAdapter(XMLStreamWriter sw)
        {
            super(sw);
        }
    }

    TestWriterAdapter() { }

    void test()
	throws XMLStreamException
    {
	XMLOutputFactory f = new com.ctc.wstx.stax.WstxOutputFactory();
	f.setProperty(XMLOutputFactory.IS_REPAIRING_NAMESPACES, Boolean.valueOf(true));
	StringWriter str = new StringWriter();
	XMLStreamWriter swOrig = f.createXMLStreamWriter(str);
	XMLStreamWriter2 sw = new MyAdapter(swOrig);

	sw.writeStartDocument();
	sw.writeStartElement("root");
	sw.writeQName(new QName("http://xxx", "elem", "prefix"));
	sw.writeEndElement();
	sw.writeEndDocument();

	sw.close();

	System.out.println("Result doc: '"+str+"'");
    }

    public static void main(String[] args)
	throws XMLStreamException
    {
	new TestWriterAdapter().test();
    }
}

