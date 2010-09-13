// $Id: ManyToMany.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;
import static javax.persistence.FetchType.*;

/**
 * Defines a many-valued association with many-to-many multiplicity. If the Collection is
 * defined using generics to specify the element type, the associated target entity class
 * does not need to be specified; otherwise it must be specified.
 *
 * Every many-to-many association has two sides, the owning side and the non-owning, or inverse,
 * side. The join table is specified on the owning side. If the association is bidirectional,
 * either side may be designated as the owning side.
 *
 * The same annotation elements for the OneToMany annotation apply to the ManyToMany annotation.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD}) @Retention(RUNTIME)
public @interface ManyToMany {
	/**
	 * The entity class that is the target of the association. Optional only if the
	 * collection property is defined using Java generics. Must be specified otherwise.
	 *
	 * Defaults to the parameterized type of the collection when defined using generics.
	 */
	Class targetEntity() default void.class;
	/**
	 * The operations that must be cascaded to the target of the association.
	 *
	 * Defaults to no operations being cascaded.
	 */
	CascadeType[] cascade() default {};
	/**
	 * Whether the association should be lazily loaded or must be eagerly fetched.
	 * The EAGER strategy is a requirement on the persistenceprovider runtime that
	 * the associatedentities must be eagerly fetched. The LAZY strategy is a hint
	 * to the persistence provider runtime.
	 */
	FetchType fetch() default LAZY;
	/**
	 * The field that owns the relationship. Required unless the relationship is unidirectional.
	 */
	String mappedBy() default "";
}
