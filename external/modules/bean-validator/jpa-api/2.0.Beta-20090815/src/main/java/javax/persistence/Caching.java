// $Id: Caching.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * @author Hardy Ferentschik
 */
public enum Caching {
	ALL,
	NONE,
	ENABLE_SELECTIVE,
	DISABLE_SELECTIVE,
	UNSPECIFIED
}
