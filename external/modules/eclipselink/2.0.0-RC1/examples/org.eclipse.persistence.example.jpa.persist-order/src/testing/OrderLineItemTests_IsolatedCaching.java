package testing;

import java.util.Map;

import org.eclipse.persistence.config.PersistenceUnitProperties;

public class OrderLineItemTests_IsolatedCaching extends OrderLineItemTests {

	@Override
	protected Map getEMFProperties() {
		Map properties = super.getEMFProperties();
		
		properties.put(PersistenceUnitProperties.CACHE_SHARED_DEFAULT, "false");
		
		return properties;
	}

}
