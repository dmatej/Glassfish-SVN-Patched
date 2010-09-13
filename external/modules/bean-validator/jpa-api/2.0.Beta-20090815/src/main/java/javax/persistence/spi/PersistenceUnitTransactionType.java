// $Id: PersistenceUnitTransactionType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.spi;

/**
 * This enum class defines whether the entity managers created by the EntityManagerFactory will be
 * JTA or resource-local entity managers.
 *
 * @author <a href="mailto:bill@jboss.org">Bill Burke</a>
 */
public enum PersistenceUnitTransactionType {
	/**
	 * JTA entity manager
	 */
	JTA,
	/**
	 * Resource-local entity manager
	 */
	RESOURCE_LOCAL
}
