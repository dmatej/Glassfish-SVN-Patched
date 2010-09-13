// $Id: TransactionRequiredException.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Thrown by the persistence provider when a transaction is required but is not active.
 * @author Gavin King
 */
public class TransactionRequiredException extends PersistenceException {

	/**
	 * Constructs a new TransactionRequiredException exception with null as its detail message
	 */
	public TransactionRequiredException() {
		super();
	}

	/**
	 * Constructs a new TransactionRequiredException exception with the specified detail message
	 * 
	 * @param message
	 */
	public TransactionRequiredException(String message) {
		super( message );
	}

}
