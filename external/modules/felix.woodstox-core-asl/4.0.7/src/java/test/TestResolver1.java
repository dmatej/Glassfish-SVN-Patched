package test;

import javax.xml.stream.*;

/**
 * Simple debug 'resolver'...
 */
public class TestResolver1
    implements XMLResolver
{
    public Object resolveEntity(String publicID, String systemID,
                                String baseURI, String namespace)
    {
        System.err.println("[XMLResolver: pub->'"+publicID+"', sys->'"+systemID
                           +"', base '"+baseURI+"', ns '"+namespace+"']");
        return null;
    }
}
