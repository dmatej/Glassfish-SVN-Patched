// $Id: ExcludeDefaultListeners.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * Specifies that the invocation of default listeners is to be excluded for the entity class
 * (or mapped superclass) and its subclasses.
 *
 * @author Emmanuel Bernard
 */
@Target(TYPE) @Retention(RUNTIME)
public @interface ExcludeDefaultListeners {
}
