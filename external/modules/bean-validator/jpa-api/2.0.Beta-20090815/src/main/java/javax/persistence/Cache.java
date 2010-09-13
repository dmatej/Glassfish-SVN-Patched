// $Id: Cache.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Interface used to interact with the second-level cache.
 * If a cache is not in use, the methods of this interface have
 * no effect, except for contains, which returns false.
 */
public interface Cache {
	/**
	 * Whether the cache contains data for the given entity.
	 */
	public boolean contains(Class cls, Object primaryKey);

	/**
	 * Remove the data for the given entity from the cache.
	 */
	public void evict(Class cls, Object primaryKey);

	/**
	 * Remove the data for entities of the specified class (and its
	 * subclasses) from the cache.
	 */
	public void evict(Class cls);

	/**
	 * Clear the cache.
	 */
	public void evictAll();
}
