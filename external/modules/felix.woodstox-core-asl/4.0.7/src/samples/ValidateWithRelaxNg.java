package samples;

import java.io.File;

import javax.xml.stream.*;

import org.codehaus.stax2.*;
import org.codehaus.stax2.validation.*;

/**
 * This is a simple example command line utility, that shows how to
 * use the new Stax2 validation API to validate input documents against
 * specific Relax NG schema instance.
 */
public class ValidateWithRelaxNg
{
    public static void main(String[] args)
    {
        if (args.length != 2) {
            System.err.println("Usage: java ValidateWithRelaxNg [input-file] [rng-file]");
            System.exit(1);
        }
        // First, let's parse RNG schema object
        XMLValidationSchemaFactory sf = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_RELAXNG);
        File schemaFile = new File(args[1]);
        XMLValidationSchema rng = null;

        try {
            rng = sf.createSchema(schemaFile);
        } catch (XMLStreamException xe) {
            System.err.println("Failed to process the RNG file ('"+schemaFile+"'): "+xe);
            System.exit(1);
        }

        // And then validate a document:
        File inputFile = new File(args[0]);
        try {
            XMLInputFactory2 ifact = (XMLInputFactory2)XMLInputFactory.newInstance();
            XMLStreamReader2 sr = ifact.createXMLStreamReader(inputFile);

            try {
                sr.validateAgainst(rng);
                /* Document validation is done as document is read through (ie.
                 * it's fully streaming as well as parsing), so just need to
                 * traverse the contents.
                 */
                while (sr.hasNext()) {
                    sr.next();
                }
            } catch (XMLValidationException vex) {
                System.err.println("Document '"+inputFile+"' failed validation: "+vex);
                System.exit(1);
            }
        } catch (XMLStreamException xse) {
            System.err.println("Failed parse the input document ('"+inputFile+"'): "+xse);
            System.exit(1);
        }
        System.out.println("Document '"+inputFile+"' succesfully validated.");
    }
}
