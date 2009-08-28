//$Id: Animal.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator.test.inheritance;

import java.io.Serializable;

/**
 * @author Emmanuel Bernard
 */
public class Animal implements Serializable, Name {
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
