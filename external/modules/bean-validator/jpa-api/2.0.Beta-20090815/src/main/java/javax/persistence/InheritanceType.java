// $Id: InheritanceType.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

/**
 * Defines inheritance strategy options.
 *
 * @author Emmanuel Bernard
 */
public enum InheritanceType
{
	/**
	 * A single table per class hierarchy
	 */
	SINGLE_TABLE,
	/**
	 * A table per concrete entity class
	 */
	TABLE_PER_CLASS,
	/**
	 * A strategy in which fields that are specific to a subclass are mapped to a separate
	 * table than the fields that are common to the parent class, and a join is performed
	 * to instantiate the subclass.
	 */
	JOINED };
