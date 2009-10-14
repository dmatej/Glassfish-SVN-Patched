//$Id: Member.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.valid;

import org.hibernate.validator.Valid;

/**
 * @author Emmanuel Bernard
 */
public class Member {

	private Address address;

	@Valid
	public Address getAddress() {
		return address;
	}


	public void setAddress(Address address) {
		this.address = address;
	}
}
