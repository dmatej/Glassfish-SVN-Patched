// $Id: Cache.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

public enum CacheRetrieveMode {

	/**
	 * Read entity data from the cache: this is
	 * the default behavior.
	 */
	USE,

	/**
	 * Bypass the cache: get data directly from
	 * the database.
	 */
	BYPASS
}
