package sahoo.metainfservicetest;

import org.osgi.framework.*;
import javax.xml.bind.JAXBContext;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

public class Activator implements BundleActivator {
    public void start(BundleContext ctx) throws Exception {
        try {
            System.out.println(Thread.currentThread().getContextClassLoader());

            JAXBContext jc = JAXBContext.newInstance(ObjectFactory.class);
            Persistence test_object = new Persistence();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            jc.createMarshaller().marshal(test_object,out);
            byte[] data = out.toByteArray();
            ByteArrayInputStream istream = new ByteArrayInputStream(data);
            Persistence out_object = (Persistence) jc.createUnmarshaller().unmarshal(istream);

            if (test_object.equals(out_object)){
                System.out.println("Marshall and Unmarshall Success");
            }
            else {
                throw new RuntimeException("Marshall/Unmarshall Failed. Exit Bundle");
            }

        } catch(Exception e) {
            e.printStackTrace();
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
