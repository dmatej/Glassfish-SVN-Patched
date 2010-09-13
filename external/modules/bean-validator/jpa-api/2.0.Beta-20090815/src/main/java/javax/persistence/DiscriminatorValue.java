// $Id: DiscriminatorValue.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.TYPE;

/**
 * Is used to specify the value of the discriminator column for entities of the given type.
 * The DiscriminatorValue annotation can only be specified on a concrete entity class.
 * If the DiscriminatorValue annotation is not specified and a discriminator column is used,
 * a provider-specific function will be used to generate a value representing the entity type.
 * If the DiscriminatorType is STRING, the discriminator value default is the entity name.
 *
 * The inheritance strategy and the discriminator column are only specified in the root
 * of an entity class hierarchy or subhierarchy in which a different inheritance strategy
 * is applied. The discriminator value, if not defaulted, should be specified for each entity
 * class in the hierarchy.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE}) @Retention(RUNTIME)
public @interface DiscriminatorValue {
	/**
	 * The value that indicates that the row is an entity of the annotated entity type.
	 * 
	 * If the DiscriminatorValue annotation is not specified and a discriminator column is used,
	 * a provider-specific function will be used to generate a value representing the entity type.
	 * If the DiscriminatorType is STRING, the discriminator value default is the entity name.
	 */
	String value();
}
