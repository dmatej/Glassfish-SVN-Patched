// $Id: EntityNotFoundException.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Thrown by the persistence provider when an entity reference obtained by
 * EntityManager.getReference(Class,Object)  is accessed but the entity does not exist.
 * Also thrown when EntityManager.refresh(Object) is called and the object no longer exists
 * in the database. The current transaction, if one is active, will be marked for rollback.
 *
 * @author Gavin King
 */
public class EntityNotFoundException extends PersistenceException {
	/**
	 * Constructs a new EntityNotFoundException exception with null as its detail message.
	 */
	public EntityNotFoundException() {
		super();
	}

	/**
	 * Constructs a new EntityNotFoundException exception with the specified detail message.
	 *
	 * @param message the detail message
	 */
	public EntityNotFoundException(String message) {
		super( message );
	}

}
