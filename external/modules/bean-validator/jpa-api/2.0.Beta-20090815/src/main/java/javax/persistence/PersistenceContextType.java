// $Id: PersistenceContextType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Specifies whether a transaction-scoped or extended persistence context is to be used in
 * PersistenceContext. If the type element is not specified, a transaction-scoped persistence
 * context is used.
 */
public enum PersistenceContextType {
	/**
	 * Transaction-scoped persistence context
	 */
	TRANSACTION,
	/**
	 * Extended persistence context
	 */
	EXTENDED
}
