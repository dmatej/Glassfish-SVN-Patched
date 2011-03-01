package samples;

import java.io.*;

import javax.xml.stream.*;

import org.codehaus.stax2.*;
import org.codehaus.stax2.validation.*;

/**
 * This is a simple example/sample class that shows how to
 * use the new Stax2 validation API to validate XML output written
 * using XMLStreamWriter, against an DTD instance.
 */
public class ValidateOutputWithDtd
{
    public static void main(String[] args)
    {
        if (args.length > 0) {
            System.err.println("Usage: java ValidateOutputWithDtd");
            System.exit(1);
        }

        final String DTD_STR =
            "<!ELEMENT root (branch | leaf)*>\n"
            +"<!ELEMENT branch (leaf)+>"
            +"<!ELEMENT leaf (#PCDATA)>"
            +"<!ATTLIST leaf desc CDATA #IMPLIED>\n";
        StringWriter strw = new StringWriter();
        try {
            // First, let's parse DTD schema object
            XMLValidationSchemaFactory sf = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_DTD);
            XMLValidationSchema dtd = sf.createSchema(new StringReader(DTD_STR));
            XMLOutputFactory ofact = XMLOutputFactory.newInstance();
            XMLStreamWriter2 sw = (XMLStreamWriter2) ofact.createXMLStreamWriter(strw);

            try {
                sw.validateAgainst(dtd); // this starts validation
                // Document validation is done as output is written
                sw.writeStartDocument();
                sw.writeStartElement("root");
                sw.writeStartElement("branch");
                sw.writeStartElement("leaf");
                sw.writeEndElement();
                // We'll get exception -- branch not legal in branch
                sw.writeStartElement("branch");
                sw.writeEndElement();
                sw.writeEndElement();
                sw.writeEndElement();
                sw.writeEndDocument();
                sw.close();
            } catch (XMLValidationException vex) {
                System.err.println("Document failed validation: "+vex);
                System.exit(1);
            }
        } catch (XMLStreamException xse) {
            System.err.println("Failed output the document: "+xse);
            System.exit(1);
        }
        // If we got this far, we are all good
        System.out.println("Document succesfully validated and written. Content: '"+strw.toString()+"'");
    }
}
