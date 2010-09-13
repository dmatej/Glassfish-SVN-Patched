// $Id: PersistenceProviderResolver.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.spi;

import java.util.List;

/**
 * Determine the list of persistence providers available in the
 * runtime environment
 * <p/>
 * Persistence providers are identified by the presence of
 * META-INF/services/javax.persistence.spi.PersistenceProvider
 * files following the Service Provider pattern.
 * <p/>
 * Each META-INF/services/javax.persistence.spi.PersistenceProvider * file contains the name of the provider implementation class of the
 * javax.persistence.spi.PersistenceProvider interface.
 * <p/>
 * Implementations must be thread-safe.
 */
public interface PersistenceProviderResolver {
	/**
	 * Returns a list of PersistenceProvider implementations
	 * available in the runtime environment.
	 *
	 * @return list of persistence providers available
	 *         in the environment
	 */
	List<PersistenceProvider> getPersistenceProviders();
}
