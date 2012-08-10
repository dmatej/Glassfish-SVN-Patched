package example.mysports.tests.admin.extensions;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.internal.jpa.metadata.xml.XMLEntityMappings;
import org.eclipse.persistence.internal.jpa.metadata.xml.XMLEntityMappingsWriter;
import org.junit.Test;

import example.mysports.admin.jaxrs.MappingsLoader;
import example.mysports.tests.TestingProperties;

public class CreateOrmXML {

    @Test
    public void createEmpty() {
        XMLEntityMappings mappings = new XMLEntityMappings();
        mappings.setVersion("2.3");

        XMLEntityMappingsWriter.write(mappings, System.out);
    }

    @Test
    public void createFromDB() {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());
        EntityManager em = emf.createEntityManager();

        try {
            String xml = MappingsLoader.getORMapping(em, "OSL");
            System.out.println(xml);

            xml = MappingsLoader.getORMapping(em, "HTHL");
            System.out.println(xml);
        } finally {
            em.close();
            emf.close();
        }
    }

}
