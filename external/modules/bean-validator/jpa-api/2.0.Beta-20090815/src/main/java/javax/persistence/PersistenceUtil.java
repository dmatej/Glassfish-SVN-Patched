// $Id: PersistenceUtil.java 17036 2009-07-08 09:09:38Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * @author Hardy Ferentschik
 */

/**
 * Utility interface between the application and the persistence
 * provider(s).
 */
public interface PersistenceUtil {
	/**
	 * Determine the load state of a given persistent attribute
	 * regardless of the persistence provider that created the
	 * containing entity.
	 * @param attributeName name of attribute whose load state is * to be determined
	 *
	 * @return false if entity's state has not been loaded or
	 *         if the attribute state has not been loaded, otherwise true
	 */
	public boolean isLoaded(Object entity, String attributeName);

	/**
	 * Determine the load state of an entity regardless
	 * of the persistence provider that created it.
	 * This method can be used to determine the load state
	 * of an entity passed as a reference. An entity is
	 * considered loaded if all attributes for which FetchType
	 * EAGER has been specified have been loaded.
	 * The isLoaded(Object, String) method should be used to
	 * determine the load state of an attribute.
	 * Not doing so might lead to unintended loading of state.
	 *
	 * @return false if the entity has not be loaded, otherwise
	 *         true.
	 */
	public boolean isLoaded(Object object);
}
