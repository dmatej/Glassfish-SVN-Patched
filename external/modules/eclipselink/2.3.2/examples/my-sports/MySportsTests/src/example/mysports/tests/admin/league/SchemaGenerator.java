package example.mysports.tests.admin.league;

import java.io.File;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.junit.Test;

import example.mysports.admin.jaxrs.MySportsContextResolver;

public class SchemaGenerator {

    @Test
    public void generateLeaguesXSD() throws Exception {
        JAXBContext context = MySportsContextResolver.createContext();
        SchemaOutputResolver sor = new MySchemaOutputResolver();
        context.generateSchema(sor);
    }

    private class MySchemaOutputResolver extends SchemaOutputResolver {
        
        public Result createOutput(String namespaceURI, String suggestedFileName) throws IOException {
            File file = new File(suggestedFileName);
            StreamResult result = new StreamResult(file);
            result.setSystemId(file.toURI().toURL().toString());
            return result;
        }
     
    }}
