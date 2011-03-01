package test;

import java.io.*;

import javax.xml.stream.*;

public class TestEventCopier
{
    final XMLInputFactory mFactory;

    private TestEventCopier()
    {
        super();
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");
        XMLInputFactory f = XMLInputFactory.newInstance();
        mFactory = f;
        //f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);
        f.setProperty(XMLInputFactory.REPORTER, new TestReporter());

        f.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, Boolean.FALSE);
        System.out.println("<!--");
        System.out.println(" Factory instance: "+f.getClass());
        System.out.println(" coalescing: "+f.getProperty(XMLInputFactory.IS_COALESCING));
        System.out.println("-->");
    }

    private void test(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+getClass().getName()+" [file]");
            System.exit(1);
        }
        String filename = args[0];
        File file = new File(filename);
        InputStream in = new FileInputStream(file);

        // Let's pass generated system id:
        XMLEventReader er = mFactory.createXMLEventReader(file.toURL().toString(), in);
        Writer out = new PrintWriter(System.out);
        XMLOutputFactory of = XMLOutputFactory.newInstance();
        XMLEventWriter ew = of.createXMLEventWriter(out);

        while (er.hasNext()) {
            ew.add(er.nextEvent());
        }
        ew.close();
    }

    public static void main(String[] args) throws Exception
    {
        new TestEventCopier().test(args);
    }
}
