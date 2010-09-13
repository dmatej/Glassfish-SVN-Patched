// $Id: SecondaryTables.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;
/**
 * This annotation is used to specify multiple secondary tables for an entity.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE}) @Retention(RUNTIME)
public @interface SecondaryTables {
	/**
	 * The secondary tables for an entity.
	 */
	SecondaryTable[] value();
}
