// $Id: LoadState.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.spi;

/**
 * @author Hardy Ferentschik
 */
public enum LoadState {
	/**
	 * the state of the element is known to have been loaded
	 */
	LOADED,
	/**
	 * the state of the element is known not to have been loaded
	 */
	NOT_LOADED,
	/**
	 * the load state of the element cannot be determined
	 */
	UNKNOWN
}

