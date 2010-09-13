// $Id: QueryHint.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * An implementation-specific Query hint
 *
 * @author Emmanuel Bernard
 */
@Target({})
@Retention(RUNTIME)
public @interface QueryHint {
	/**
	 * Name of the hint
	 */
	String name();

	/**
	 * Value of the hint
	 */
	String value();
}
