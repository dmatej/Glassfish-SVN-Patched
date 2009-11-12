//$Id: Commander.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.jpa;

import javax.persistence.Embeddable;
import javax.persistence.Column;

import org.hibernate.validator.NotEmpty;

/**
 * @author Emmanuel Bernard
 */
@Embeddable
public class Commander {
	@NotEmpty
	@Column(name="commander_name")
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
