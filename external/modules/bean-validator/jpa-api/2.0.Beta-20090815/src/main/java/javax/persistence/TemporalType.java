// $Id: TemporalType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Type used to indicate a specific mapping of Date or Calendar.
 */
public enum TemporalType {
	/**
	 * Map as java.sql.Date
	 */
	DATE,
	/**
	 * Map as java.sql.Time
	 */
	TIME,
	/**
	 * Map as java.sql.Timestamp
	 */
	TIMESTAMP
}
