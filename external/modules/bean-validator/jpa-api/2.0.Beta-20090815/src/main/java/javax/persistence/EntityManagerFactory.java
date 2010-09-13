// $Id: EntityManagerFactory.java 17305 2009-08-14 18:29:45Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.util.Map;
import java.util.Set;
import javax.persistence.criteria.QueryBuilder;
import javax.persistence.metamodel.Metamodel;

/**
 * Interface used to interact with the entity manager factory * for the persistence unit.
 */
public interface EntityManagerFactory {
	/**
	 * Create a new EntityManager.
	 * This method returns a new EntityManager instance each time
	 * it is invoked.
	 * The isOpen method will return true on the returned instance.
	 *
	 * @throws IllegalStateException if the entity manager factory
	 *                               has been closed.
	 */
	public EntityManager createEntityManager();

	/**
	 * Create a new EntityManager with the specified Map of
	 * properties.
	 * This method returns a new EntityManager instance each time
	 * it is invoked.
	 * The isOpen method will return true on the returned instance.
	 *
	 * @throws IllegalStateException if the entity manager factory
	 *                               has been closed.
	 */
	public EntityManager createEntityManager(Map map);

	/**
	 * Return an instance of QueryBuilder for the creation of
	 * Criteria API objects.
	 *
	 * @return QueryBuilder instance
	 *
	 * @throws IllegalStateException if the entity manager factory
	 *                               has been closed.
	 */
	public QueryBuilder getQueryBuilder();

	/**
	 * Return an instance of Metamodel interface for access to the
	 * metamodel of the persistence unit.
	 *
	 * @return Metamodel instance
	 *
	 * @throws IllegalStateException if the entity manager has
	 *                               been closed.
	 */
	public Metamodel getMetamodel();

	/**
	 * Indicates whether the factory is open. Returns true
	 * until the factory has been closed.
	 */
	public boolean isOpen();

	/**
	 * Close the factory, releasing any resources that it holds.
	 * After a factory instance is closed, all methods invoked on
	 * it will throw an IllegalStateException, except for isOpen,
	 * which will return false. Once an EntityManagerFactory has
	 * been closed, all its entity managers are considered to be
	 * in the closed state.
	 *
	 * @throws IllegalStateException if the entity manager factory
	 *                               has been closed.
	 */
	public void close();

	/**
	 * Get the properties and associated values that are in effect
	 * for the entity manager factory. Changing the contents of the
	 * map does not change the configuration in effect.
	 *
	 * @return properties
	 */
	public Map<String, Object> getProperties();

	/**
	 * Get the names of the properties that are supported for use
	 * with the entity manager factory. These correspond to
	 * properties that may be passed to the methods of the
	 * EntityManagerFactory interface that take a properties
	 * argument. These include all standard properties as well as
	 * vendor-specific properties supported by the provider. These
	 * properties may or may not currently be in effect.
	 *
	 * @return properties and hints
	 */
	public Set<String> getSupportedProperties();

	/**
	 * Access the cache that is associated with the entity manager
	 * factory (the "second level cache").
	 *
	 * @return instance of the Cache interface
	 *
	 * @throws IllegalStateException if the entity manager factory
	 *                               has been closed.
	 */
	public Cache getCache();

	/**
     * Return interface providing access to utility methods
     * for the persistence unit.
     * @return PersistenceUnitUtil interface
     * @throws IllegalStateException if the entity manager factory
     * has been closed.
     */
    public PersistenceUnitUtil getPersistenceUnitUtil();
}
