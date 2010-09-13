// $Id: AttributeOverrides.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;


/**
 * Is used to override mappings of multiple properties or fields
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface AttributeOverrides {
	/**
	 * One or more mapping override
	 */
	AttributeOverride[] value();
}
