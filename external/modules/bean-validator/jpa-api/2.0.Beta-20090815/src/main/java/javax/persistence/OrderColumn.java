// $Id: OrderColumn.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * @author Hardy Ferentschik
 */
@Target({ METHOD, FIELD })
@Retention(RUNTIME)
public @interface OrderColumn {
	String name() default "";

	boolean nullable() default true;

	boolean insertable() default true;

	boolean updatable() default true;

	String columnDefinition() default "";

	int base() default 0;

	String table() default "";
}
