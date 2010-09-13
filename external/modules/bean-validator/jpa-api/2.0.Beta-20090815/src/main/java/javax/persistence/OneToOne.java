// $Id: OneToOne.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;
import static javax.persistence.FetchType.*;

/**
 * This annotation defines a single-valued association to another entity that has
 * one-to-one multiplicity. It is not normally necessary to specify the associated
 * target entity explicitly since it can usually be inferred from the type of the object
 * being referenced.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD}) @Retention(RUNTIME)
public @interface OneToOne {
	/**
	 * The entity class that is the target of the association.
	 *
	 * Defaults to the type of the field or property that stores the association.
	 */
	Class targetEntity() default void.class;
	/**
	 * The operations that must be cascaded to the target of the association.
	 *
	 * By default no operations are cascaded.
	 */
	CascadeType[] cascade() default {};
	/**
	 * Whether the association should be lazily loaded or must be eagerly fetched.
	 * The EAGER strategy is a requirement on the persistence provider runtime that
	 * the associated entity must be eagerly fetched. The LAZY strategy is a hint to
	 * the persistence provider runtime.
	 */
	FetchType fetch() default EAGER;
	/**
	 * Whether the association is optional. If set to false then a non-null relationship must
	 * always exist.
	 */
	boolean optional() default true;
	/**
	 * The field that owns the relationship. This element is only specified on the
	 * inverse (non-owning) side of the association.
	 */
	String mappedBy() default "";
}
