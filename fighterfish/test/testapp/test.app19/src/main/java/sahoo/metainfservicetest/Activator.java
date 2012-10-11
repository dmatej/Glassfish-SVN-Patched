package sahoo.metainfservicetest;

import org.osgi.framework.*;
import javax.xml.bind.JAXBContext;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

public class Activator implements BundleActivator {
    public void start(BundleContext ctx) throws Exception {
        try {
            System.out.println("MY CLASSLOADER " + Thread.currentThread().getContextClassLoader());

            JAXBContext jc = JAXBContext.newInstance(Persistence.class);
            Persistence test_object = new Persistence();
            test_object.setVersion("3.0");
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            jc.createMarshaller().marshal(test_object,out);
            byte[] data = out.toByteArray();
            ByteArrayInputStream istream = new ByteArrayInputStream(data);
            Persistence out_object = (Persistence) jc.createUnmarshaller().unmarshal(istream);

            final String O2_version = out_object.getVersion();

             if("3.0".equals(O2_version)){System.out.println("Marshall and UnMarshall Success.");}

             else { throw new RuntimeException("Marshall/UnMarshall of Persistence Object Failed.");}

        } catch(Exception e) {
             throw new RuntimeException("Marshall/UnMarshall of Persistence Object Failed.");
        }

        // This works, because GlassFish uses StAX from JRE.
        try {
            javax.xml.stream.XMLInputFactory.newInstance();
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
 
    public void stop(BundleContext ctx) {
    }
}
