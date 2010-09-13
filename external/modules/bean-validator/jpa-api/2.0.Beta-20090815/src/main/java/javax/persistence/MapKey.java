// $Id: MapKey.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;
import java.lang.annotation.Retention;

/**
 * Is used to specify the map key for associations of type Map.
 * If a persistent field or property other than the primary key is used as a map key then it
 * is expected to have a uniqueness constraint associated with it.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD}) @Retention(RUNTIME)
public @interface MapKey {
	/**
	 * The name of the persistent field or property of the associated entity that is used as the map key.
	 * If the name element is not specified, the primary key of the associated entity is used as the map key.
	 * If the primary key is a composite primary key and is mapped as IdClass, an instance of the primary key
	 * class is used as the key.
	 */
	String name() default "";
}
