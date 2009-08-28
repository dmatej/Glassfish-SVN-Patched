//$Id: Name.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator.test.inheritance;

import org.hibernate.validator.NotNull;

/**
 * @author Emmanuel Bernard
 */
public interface Name {
	@NotNull
	String getName();

	void setName(String name);
}
