// $Id: Enumerated.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;
import static javax.persistence.EnumType.*;

/**
 * Specifies that a persistent property or field should be persisted as a enumerated type.
 * It may be used in conjunction with the Basic annotation.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface Enumerated {
	/**
	 * The type used in mapping an enum type
	 */
	EnumType value() default ORDINAL;
}
