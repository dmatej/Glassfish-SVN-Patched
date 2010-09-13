// $Id: EnumType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Defines mapping for the enumerated types. The constants of this enumerated type specify how persistent
 * property or field should be persisted as a enumerated type.
 *
 * @author Emmanuel Bernard
 */
public enum EnumType {
	/**
	 * Persist enumerated type property or field as an integer
	 */
	ORDINAL,
	/**
	 * Persist enumerated type property or field as a string
	 */
	STRING
}
