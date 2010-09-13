// $Id: EntityTransaction.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * The EntityTransaction interface is used to control resource transactions
 * on resource-local entity managers. The EntityManager.getTransaction()
 * method returns the EntityTransaction interface.
 *
 * @author Emmanuel Bernard
 */
public interface EntityTransaction {
	/**
	 * Start a resource transaction.
	 *
	 * @throws IllegalStateException if isActive() is true.
	 */
	public void begin();

	/**
	 * Commit the current transaction, writing any unflushed
	 * changes to the database.
	 *
	 * @throws IllegalStateException if isActive() is false.
	 * @throws RollbackException	 if the commit fails.
	 */
	public void commit();

	/**
	 * Roll back the current transaction.
	 *
	 * @throws IllegalStateException if isActive() is false.
	 * @throws PersistenceException  if an unexpected error
	 *                               condition is encountered.
	 */
	public void rollback();

	/**
	 * Mark the current transaction so that the only possible
	 * outcome of the transaction is for the transaction to be
	 * rolled back.
	 *
	 * @throws IllegalStateException if isActive() is false.
	 */
	public void setRollbackOnly();

	/**
	 * Determine whether the current transaction has been marked
	 * for rollback.
	 *
	 * @throws IllegalStateException if isActive() is false.
	 */
	public boolean getRollbackOnly();

	/**
	 * Indicate whether a transaction is in progress.
	 *
	 * @throws PersistenceException if an unexpected error
	 *                              condition is encountered.
	 */
	public boolean isActive();
}
