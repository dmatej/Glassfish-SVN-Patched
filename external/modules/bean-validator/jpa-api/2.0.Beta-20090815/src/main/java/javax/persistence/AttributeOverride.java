// $Id: AttributeOverride.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;


/**
 * The AttributeOverride annotation is used to override the mapping of a Basic (whether explicit
 * or default) property or field or Id property or field.
 *
 * The AttributeOverride annotation may be applied to an entity that extends a mapped superclass
 * or to an embedded field or property to override a basic mapping defined by the mapped superclass
 * or embeddable class. If the AttributeOverride annotation is not specified, the column is mapped
 * the same as in the original mapping.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface AttributeOverride {
	/**
	 * The name of the property whose mapping is being overridden if property-based access is being
	 * used, or the name of the field if field-based access is used.
	 */
	String name();
	/**
	 * The column that is being mapped to the persistent attribute
	 */
	Column column();
}
