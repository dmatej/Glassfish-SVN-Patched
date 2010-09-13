// $Id: AssociationOverrides.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;

/**
 * This annotation is used to override mappings of multiple many-to-one
 * or one-to-one relationship properties or fields.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface AssociationOverrides {
	/**
	 * Mapping overrides of relationship properties or fields
	 */
	AssociationOverride[] value();
}
