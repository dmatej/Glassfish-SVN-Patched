// $Id: OptimisticLockException.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Thrown by the persistence provider when an optimistic locking conflict occurs.
 * This exception may be thrown as part of an API call, a flush or at commit time.
 * The current transaction, if one is active, will be marked for rollback.
 *
 * @author Emmanuel Bernard
 */
public class OptimisticLockException extends PersistenceException {
	private Object entity;

	public OptimisticLockException() {
		super();
	}

	public OptimisticLockException(Object entity) {
		this.entity = entity;
	}

	public OptimisticLockException(Throwable cause) {
		super( cause );
	}

	public OptimisticLockException(String message) {
		super( message );
	}

	public OptimisticLockException(String message, Throwable cause) {
		super( message, cause );
	}

	public OptimisticLockException(String message, Throwable cause, Object entity) {
		super( message, cause );
		this.entity = entity;
	}

	public Object getEntity() {
		return entity;
	}
}
