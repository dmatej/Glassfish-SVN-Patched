//$Id: Address.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.valid;

import org.hibernate.validator.NotNull;

/**
 * @author Emmanuel Bernard
 */
public class Address {

	private String city;

	@NotNull
	public String getCity() {
		return city;
	}
} 
