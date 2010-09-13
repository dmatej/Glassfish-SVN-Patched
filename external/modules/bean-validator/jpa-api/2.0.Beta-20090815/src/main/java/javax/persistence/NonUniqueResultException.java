// $Id: NonUniqueResultException.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Thrown by the persistence provider when getSingleResult() is executed on a query and there is more than
 * one result from the query. This exception will not cause the current transaction, if one is active, to be
 * marked for roll back.
 *
 * @author Gavin King
 */
public class NonUniqueResultException extends PersistenceException {

	/**
	 * Constructs a new NonUniqueResultException exception with null as its detail message
	 */
	public NonUniqueResultException() {
		super();
	}

	/**
	 * Constructs a new NonUniqueResultException exception with the specified detail message
	 * 
	 * @param message
	 */
	public NonUniqueResultException(String message) {
		super( message );
	}

}
