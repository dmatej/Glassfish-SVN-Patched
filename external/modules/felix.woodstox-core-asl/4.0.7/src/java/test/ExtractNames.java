package test;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.*;

/**
 * This is a simple utility that allows for extracting all qnames from
 * set of xml documents (recursively, if directories are passed)
 */
public final class ExtractNames
    implements XMLStreamConstants
{
    ExtractNames() { }

    void extract(String[] args)
        throws Exception
    {
        if (args.length < 1) {
            System.err.println("Usage: java "+getClass().getName()+" [file_or_dir1] ... [file_or_dirN]");
            System.exit(1);
        }
        XMLInputFactory2 fact = new com.ctc.wstx.stax.WstxInputFactory();
        for (int i = 0; i < args.length; ++i) {
            extract2(fact, new File(args[i]), true);
        }
    }

    void extract2(XMLInputFactory2 factory, File fileOrDir, boolean root)
        throws Exception
    {
        if (fileOrDir.isDirectory()) {
            File[] files = fileOrDir.listFiles();
            for (int i = 0; i < files.length; ++i) {
                extract2(factory, files[i], false);
            }
            return;
        }
        /* Quick additional heuristic: let's only include *.xml files
         * if this was indirectly included (being included in a directory
         * that was included)
         */
        if (!root && !fileOrDir.getName().endsWith(".xml")) {
            System.err.println("Warning: skipping non-XML (?) file '"+fileOrDir.getAbsolutePath()+"'");
            return;
        }

        XMLStreamReader2 sr = factory.createXMLStreamReader(fileOrDir);
        String lastName = "";
        while (sr.hasNext()) {
            switch (sr.next()) {
            case START_ELEMENT: // END_ELEMENT is not needed
                {
                    // Let's reduce number of names slightly
                    String name = sr.getPrefixedName();
                    if (!name.equals(lastName)) {
                        System.out.println(name);
                        lastName = name;
                    }
                    int attrCount = sr.getAttributeCount();
                    for (int i = 0; i < attrCount; ++i) {
                        String ln = sr.getAttributeLocalName(i);
                        String prefix = sr.getAttributePrefix(i);
                        if (prefix != null) {
                            System.out.print(prefix);
                            System.out.print(':');
                        }
                        System.out.println(ln);
                    }
                }
                break;
            case PROCESSING_INSTRUCTION:
                System.out.println(sr.getPITarget());
            }
        }
    }

    public static void main(String[] args)
        throws Exception
    {
        new ExtractNames().extract(args);
    }
}

