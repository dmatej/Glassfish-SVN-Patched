// $Id: MappedSuperclass.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * Designates a class whose mapping information is applied to the entities that inherit
 * from it. A mapped superclass has no separate table defined for it.
 *
 * A class designated with the MappedSuperclass annotation can be mapped in the same way as
 * an entity except that the mappings will apply only to its subclasses since no table exists
 * for the mapped superclass itself. When applied to the subclasses the inherited mappings will
 * apply in the context of the subclass tables. Mapping information may be overridden in such
 * subclasses by using the AttributeOverride and AssociationOverride annotations or corresponding *
 * XML elements.
 *  
 * @author Emmanuel Bernard
 */
@Target(TYPE) @Retention(RUNTIME)
public @interface MappedSuperclass {}
