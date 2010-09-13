// $Id: FetchType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Defines strategies for fetching data from the database.
 * The EAGER strategy is a requirement on the persistence provider runtime that data must
 * be eagerly fetched. The LAZY strategy is a hint to the persistence provider runtime that
 * data should be fetched lazily when it is first accessed. The implementation is permitted to
 * eagerly fetch data for which the LAZY strategy hint has been specified. In particular, lazy
 * fetching might only be available for Basic mappings for which property-based access is used.
 *
 * @author Emmanuel Bernard
 */
public enum FetchType {
	/**
	 * Defines that data must be lazily fetched
	 */
	LAZY,
	/**
	 * Defines that data must be eagerly fetched
	 */
	EAGER
};
