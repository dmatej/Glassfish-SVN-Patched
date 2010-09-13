// $Id: EntityExistsException.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Thrown by the persistence provider when EntityManager.persist(Object) is called and the
 * entity already exists. The current transaction, if one is active, will be marked for rollback.
 *
 * @author Emmanuel Bernard
 */
public class EntityExistsException extends PersistenceException {
	/**
	 * Constructs a new EntityExistsException exception with null as its detail message.
	 */
	public EntityExistsException() {
		super();
	}

	/**
	 * Constructs a new EntityExistsException exception with the specified cause.
	 *
	 * @param cause the cause
	 */
	public EntityExistsException(Throwable cause) {
		super( cause );
	}

	/**
	 * Constructs a new EntityExistsException exception with the specified detail message.
	 *
	 * @param message the detail message.
	 */
	public EntityExistsException(String message) {
		super( message );
	}

	/**
	 * Constructs a new EntityExistsException exception with the specified detail message and cause.
	 *
	 * @param message the detail message.
	 * @param cause the cause.
	 */
	public EntityExistsException(String message, Throwable cause) {
		super( message, cause );
	}
}
