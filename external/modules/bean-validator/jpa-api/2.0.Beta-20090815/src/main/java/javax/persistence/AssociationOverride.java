// $Id: AssociationOverride.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * This annotation is used to override a many-to-one or one-to-one mapping of property or field for
 * an entity relationship.
 * The AssociationOverride annotation may be applied to an entity that extends a mapped superclass
 * to override a many-to-one or one-to-one mapping defined by the mapped superclass. If the
 * AssociationOverride annotation is not specified, the join column is mapped the same as in
 * the original mapping.
 *
 * @author Emmanuel Bernard
 */
@Target({ TYPE, METHOD, FIELD })
@Retention(RUNTIME)
public @interface AssociationOverride {
	String name();

	JoinColumn[] joinColumns() default { };

	JoinTable joinTable() default @JoinTable;
}
