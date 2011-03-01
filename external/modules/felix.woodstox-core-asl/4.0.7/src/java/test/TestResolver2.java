package test;

import javax.xml.stream.*;

/**
 * Simple debug 'resolver'...
 */
public class TestResolver2
    implements XMLResolver
{
    public Object resolveEntity(String publicId, String systemId,
                                String baseURL, String name)
    {
        System.err.println("[TestResolver2: '"+publicId+"'/'"+systemId
                           +", baseURL "+baseURL+", entity '"+name+"']");
        return null;
    }
}
