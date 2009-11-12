//$Id: PropertyConstraint.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator;

import org.hibernate.mapping.Property;

/**
 * Interface implemented by the validator
 * when a constraint may be represented in a
 * hibernate metadata property
 *
 * @author Gavin King
 */
public interface PropertyConstraint {

	public void apply(Property property);
}
