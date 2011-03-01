package test;

import java.io.*;
import java.util.List;
import java.util.zip.GZIPInputStream;

import javax.xml.stream.*;

import org.codehaus.stax2.DTDInfo;
import org.codehaus.stax2.XMLInputFactory2;
import org.codehaus.stax2.XMLStreamReader2;
import org.codehaus.stax2.evt.NotationDeclaration2;

import com.ctc.wstx.api.WstxInputProperties;

/**
 * Simple helper test class for checking how stream reader handles xml
 * documents.
 */
public class TestStreamReader
    implements XMLStreamConstants
{
    protected TestStreamReader() {
    }

    protected XMLInputFactory2 getFactory()
    {
        System.setProperty("javax.xml.stream.XMLInputFactory",
                           "com.ctc.wstx.stax.WstxInputFactory");

        XMLInputFactory2 f =  (XMLInputFactory2) XMLInputFactory.newInstance();
        System.out.println("Factory instance: "+f.getClass());

        //f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        f.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);

        //f.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, Boolean.TRUE);
        f.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, Boolean.FALSE);

        f.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES,
                      Boolean.FALSE
                      //Boolean.TRUE
                      );

        f.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.TRUE);
        //f.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.FALSE);

        f.setProperty(XMLInputFactory.IS_VALIDATING, Boolean.FALSE);
        //f.setProperty(XMLInputFactory.IS_VALIDATING, Boolean.TRUE);

        f.setProperty(XMLInputFactory.RESOLVER, new TestResolver1());
        if (f.isPropertySupported(XMLInputFactory2.P_REPORT_PROLOG_WHITESPACE)) {
            f.setProperty(XMLInputFactory2.P_REPORT_PROLOG_WHITESPACE,
                          Boolean.FALSE
                          //Boolean.TRUE
            );
        }

        f.setProperty(XMLInputFactory2.XSP_SUPPORT_XMLID,
                      XMLInputFactory2.XSP_V_XMLID_TYPING
                      //XMLInputFactory2.XSP_V_XMLID_NONE
                      );

        if (f.isPropertySupported(WstxInputProperties.P_MIN_TEXT_SEGMENT)) {
            f.setProperty(WstxInputProperties.P_MIN_TEXT_SEGMENT,
                          new Integer(1));
            //new Integer(23));
        }

        //f.setProperty(XMLInputFactory2.P_LAZY_PARSING, Boolean.FALSE);

        /*
        if (f.isPropertySupported(WstxInputProperties.P_CUSTOM_INTERNAL_ENTITIES)) {
            java.util.Map m = new java.util.HashMap();
            m.put("myent", "foobar");
            m.put("myent2", "<tag>R&amp;B + &myent;</tag>");
            f.setProperty(WstxInputProperties.P_CUSTOM_INTERNAL_ENTITIES, m);
        }
        */

        if (f.isPropertySupported(WstxInputProperties.P_DTD_RESOLVER)) {
            f.setProperty(WstxInputProperties.P_DTD_RESOLVER,
                          new TestResolver2());
        }
        if (f.isPropertySupported(WstxInputProperties.P_ENTITY_RESOLVER)) {
            f.setProperty(WstxInputProperties.P_ENTITY_RESOLVER,
                          new TestResolver2());
        }

        /* Uncomment for boundary-condition stress tests; should be ok to 
         * use some fairly small (but not tiny) number...
         */
        if (f.isPropertySupported(WstxInputProperties.P_INPUT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_INPUT_BUFFER_LENGTH,
                          new Integer(32));
        }

        /*
        if (f.isPropertySupported(WstxInputProperties.P_TEXT_BUFFER_LENGTH)) {
            f.setProperty(WstxInputProperties.P_TEXT_BUFFER_LENGTH,
                          new Integer(17));
        }
        */

        f.setProperty(WstxInputProperties.P_INPUT_PARSING_MODE,
                      //WstxInputProperties.PARSING_MODE_FRAGMENT
                      //WstxInputProperties.PARSING_MODE_DOCUMENTS
                      WstxInputProperties.PARSING_MODE_DOCUMENT
                      );

        return f;
    }

    protected int test(File file)
        throws Exception
    {
        XMLInputFactory2 f = getFactory();

        //f.setProperty(WstxInputProperties.P_BASE_URL, "file:///tmp/");

        System.out.print("Coalesce: "+f.getProperty(XMLInputFactory.IS_COALESCING));
        System.out.println("NS-aware: "+f.getProperty(XMLInputFactory.IS_NAMESPACE_AWARE));
        System.out.print("Entity-expanding: "+f.getProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES));
        System.out.println("Validating: "+f.getProperty(XMLInputFactory.IS_VALIDATING));
        System.out.println("Xml-id support: "+f.getProperty(XMLInputFactory2.XSP_SUPPORT_XMLID));

        int total = 0;
        XMLStreamReader2 sr;

        // Let's deal with gzipped files too?
        if (file.getName().endsWith(".gz")) {
            System.out.println("[gzipped input file!]");
            sr = (XMLStreamReader2) f.createXMLStreamReader
                (new InputStreamReader(new GZIPInputStream
                                       (new FileInputStream(file)), "UTF-8"));
        } else {
            sr = (XMLStreamReader2) f.createXMLStreamReader(file);
            //sr = (XMLStreamReader2) f.createXMLStreamReader(new InputStreamReader(new FileInputStream(file), "IBM500"));
            //sr = (XMLStreamReader2) f.createXMLStreamReader(new StreamSource(file));
        }

        //sr.setProperty(WstxInputProperties.P_BASE_URL, "file:///tmp");

        System.err.println("Base URL: "+sr.getProperty(WstxInputProperties.P_BASE_URL));

        int type = sr.getEventType();

        System.out.println("START: version = '"+sr.getVersion()
                           +"', xml-encoding = '"+sr.getCharacterEncodingScheme()
                           +"', input encoding = '"+sr.getEncoding()+"'");

        //while (sr.hasNext()) {
        while (type != END_DOCUMENT) {
            type = sr.next();
            total += type; // so it won't be optimized out...

            boolean hasName = sr.hasName();

            System.out.print("["+type+"]");

            // Uncomment for location info debugging:
	    /*
            LocationInfo li = sr.getLocationInfo();
            System.out.println(" BEGIN: "+li.getStartLocation());
            //System.out.println(" CURR:  "+li.getCurrentLocation());
            System.out.println(" END:   "+li.getEndLocation());
	    */

            if (sr.hasText()) {
                String text = null;

                // Choose normal or streaming
                if (true) {
                    text = sr.getText();
                } else {
                    StringWriter swr = new StringWriter();
                    int gotLen = sr.getText(swr, false);
                    text = swr.toString();
                    if (gotLen != text.length()) {
                        throw new Error("Error: lengths didn't match: getText() returned "+gotLen+", but String has "+text.length()+" chars.");
                    }
                }

                if (text != null) { // Ref. impl. returns nulls sometimes
                    total += text.length(); // to prevent dead code elimination
                }
                if (type == CHARACTERS || type == CDATA || type == COMMENT) {
                    System.out.println(" Text("+text.length()+") = '"+text+"'.");
                    if (text.length() == 1) {
                        System.out.println(" [first char code: 0x"+Integer.toHexString(text.charAt(0))+"]");
                    }
                } else if (type == SPACE) {
                    System.out.print(" Ws = '"+text+"'.");
                    char c = (text.length() == 0) ? ' ': text.charAt(text.length()-1);
                    if (c != '\r' && c != '\n') {
                        System.out.println();
                    }
                } else if (type == DTD) {
                    DTDInfo info = sr.getDTDInfo();
                    System.out.println(" DTD (root "
                                       +getNullOrStr(info.getDTDRootName())
                                       +", sysid "+getNullOrStr(info.getDTDSystemId())
                                       +", pubid "+getNullOrStr(info.getDTDPublicId())
                                       +");");
                    List entities = (List) sr.getProperty("javax.xml.stream.entities");
                    List notations = (List) sr.getProperty("javax.xml.stream.notations");
                    int entCount = (entities == null) ? -1 : entities.size();
                    int notCount = (notations == null) ? -1 : notations.size();
                    System.out.print("  ("+entCount+" entities, "+notCount
                                       +" notations), sysid ");
                    if (notCount > 0) {
                        System.out.println();
                        for (int i = 0; i < notCount; ++i) {
                            NotationDeclaration2 nd = (NotationDeclaration2)notations.get(i);
                            System.out.println(" notation '"+nd.getName()+"', base: ["+nd.getBaseURI()+"]");
                        }
                    }
                    System.out.print(", declaration = <<");
                    System.out.print(text);
                    System.out.println(">>");
                } else if (type == ENTITY_REFERENCE) {
                    // entity ref
                    System.out.println(" Entity ref: &"+sr.getLocalName()+" -> '"+sr.getText()+"'.");
                    hasName = false; // to suppress further output
                } else { // PI?
                    ;
                }

                if (type == CHARACTERS) {
                    boolean isSpace = sr.isWhiteSpace();
                    int len = sr.getTextLength();

                    if (isSpace) {
                        text = sr.getText();
                        StringBuffer sb = new StringBuffer();
                        for (int i = 0; i < len; ++i) {
                            char c = text.charAt(i);
                            if (c == ' ') {
                                sb.append("\\s");
                            } else if (c == '\t') {
//if(true) throw new Error("TAB!");
                                sb.append("\\t");
                            } else if (c == '\r') {
                                sb.append("\\r");
                            } else if (c == '\n') {
                                sb.append("\\n");
                            } else {
                                sb.append("?");
                            }
                        }
                        text = sb.toString();
                        System.out.println("[SC:"+text+"]");
                    }
                }
            }

            if (type == PROCESSING_INSTRUCTION) {
                System.out.println(" PI target = '"+sr.getPITarget()+"'.");
                System.out.println(" PI data = '"+sr.getPIData()+"'.");
            } else if (type == START_ELEMENT) {
                String prefix = sr.getPrefix();
                System.out.print('<');
                if (prefix == null) {
                    System.out.print("{null-prefix}");
                } else if (prefix.length() == 0) {
                    System.out.print("{empty-prefix}");
                } else {
                    System.out.print(prefix);
                }
                System.out.print(sr.getLocalName());
                //System.out.println("[first char 0x"+Integer.toHexString(sr.getLocalName().charAt(0))+"]");
                System.out.print(" {ns '");
                System.out.print(sr.getNamespaceURI());
                System.out.print("'}> ");
                int count = sr.getAttributeCount();
                int nsCount = sr.getNamespaceCount();
                int idIx = sr.getAttributeInfo().getIdAttributeIndex();
                System.out.println(" ["+nsCount+" ns, "+count+" attrs, id: "
                                   +((idIx < 0) ? "none" : ("#"+idIx))+"]");
                // debugging:
                for (int i = 0; i < nsCount; ++i) {
                    System.out.println(" ns#"+i+": '"+sr.getNamespacePrefix(i)
                                     +"' -> '"+sr.getNamespaceURI(i)
                                     +"'");
                }
                for (int i = 0; i < count; ++i) {
                    String val = sr.getAttributeValue(i);
                    System.out.print(" attr#"+i+"("+sr.getAttributeType(i)+"): "+sr.getAttributePrefix(i)
                                     +":"+sr.getAttributeLocalName(i)
                                     +" ("+sr.getAttributeNamespace(i)
                                     +") -> '"+val
                                     +"' ["+(val.length())+"]");
                    System.out.println(sr.isAttributeSpecified(i) ?
                                       "[specified]" : "[Default]");
                }
                System.out.print(" [Loc -> "+sr.getLocation()+"]");
            } else if (type == END_ELEMENT) {
                System.out.print("</");
                String prefix = sr.getPrefix();
                if (prefix != null && prefix.length() > 0) {
                    System.out.print(prefix);
                    System.out.print(':');
                }
                System.out.print(sr.getLocalName());
                System.out.print(" {ns '");
                System.out.print(sr.getNamespaceURI());
                System.out.print("'}> ");
                int nsCount = sr.getNamespaceCount();
                System.out.print(" ["+nsCount+" ns unbound]");
                System.out.print(" [Loc -> "+sr.getLocation()+"]");
                System.out.println();
            } else if (type == START_DOCUMENT) { // only for multi-doc mode
                System.out.print("XML-DECL: version = '"+sr.getVersion()
                                 +"', xml-decl-encoding = '"+sr.getCharacterEncodingScheme()
                                 +"', app-encoding = '"+sr.getEncoding()
                                 +"', stand-alone set: "+sr.standaloneSet());
            }
        }
        return total;
    }

    static String getNullOrStr(String input) {
        return (input == null) ? "[NULL]" : ("'"+input+"'");
    }

    public static void main(String[] args)
        throws Exception
    {
        if (args.length != 1) {
            System.err.println("Usage: java ... "+TestStreamReader.class+" [file]");
            System.exit(1);
        }

        try {
            int total = new TestStreamReader().test(new File(args[0]));
            System.out.println();
            System.out.println("Total: "+total);
        } catch (Throwable t) {
          System.err.println("Error: "+t);
          t.printStackTrace();
        }
    }
}
