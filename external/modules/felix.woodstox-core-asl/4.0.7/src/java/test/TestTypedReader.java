package test;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.typed.TypedXMLStreamReader;

import com.ctc.wstx.stax.WstxInputFactory;

/**
 * Simple helper test class for checking how stream reader handles xml
 * documents.
 */
public class TestTypedReader
    implements XMLStreamConstants
{
    public void test()
        throws XMLStreamException
    {
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");

        XMLInputFactory f = new WstxInputFactory();

        String xml = "<root>"
            +"1 2 3 4 5 61111 -9832<?pi?> 15\n\r <child /> <!-- yay   -->  4\n"
            + "</root>";

        int[] result = new int[20];

        TypedXMLStreamReader r = (TypedXMLStreamReader) f.createXMLStreamReader(new StringReader(xml));
        r.next();

        int count = r.readElementAsIntArray(result, 0, 20);

        System.out.println("Pass 1, Ints found: "+count);
        for (int i = 0; i < count; ++i) {
            System.out.println(" #"+i+" -> "+result[i]);
        }
        r.close();

        // Then one by one:
        r = (TypedXMLStreamReader) f.createXMLStreamReader(new StringReader(xml));
        r.next();
        int index = 0;

        System.out.println("Pass 2, one by one access...");
        while (true) {
            count = r.readElementAsIntArray(result, 0, 1);
            if (count < 0) {
                System.out.println("EOF");
                break;
            }
            if (count != 1) {
                throw new IllegalStateException("Weird return value: "+count);
            }
            System.out.println(" #"+index+" -> "+result[0]);
            ++index;
        }
        r.close();
    }

    static String getNullOrStr(String input) {
        return (input == null) ? "[NULL]" : ("'"+input+"'");
    }

    public static void main(String[] args)
        throws Exception
    {
        new TestTypedReader().test();
    }
}
