// $Id: OrderBy.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation specifies the ordering of the elements of a collection valued association at the
 * point when the association is retrieved.
 *
 * The syntax of the value ordering element is an orderby_list, as follows:
 *   <code>orderby_list::= orderby_item [,orderby_item]*
 *  orderby_item::= property_or_field_name [ASC | DESC]</code>
 *
 * If ASC or DESC is not specified, ASC (ascending order) is assumed.
 *
 * If the ordering element is not specified, ordering by the primary key of the associated
 * entity is assumed.
 *
 * The property or field name must correspond to that of a persistent property or field of the
 * associated class. The properties or fields used in the ordering must correspond to columns
 * for which comparison operators are supported.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD}) @Retention(RUNTIME)
public @interface OrderBy {
	/**
	 * An orderby_list, specified as follows:
	 * orderby_list::= orderby_item [,orderby_item]* orderby_item::= property_or_field_name [ASC | DESC]
	 *
	 * If ASC or DESC is not specified, ASC (ascending order) is assumed.
	 *
	 */
	String value() default "";
}
