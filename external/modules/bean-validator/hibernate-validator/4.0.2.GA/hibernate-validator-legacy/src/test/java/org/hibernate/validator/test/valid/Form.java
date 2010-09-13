//$Id: Form.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.valid;

import org.hibernate.validator.Valid;

/**
 * @author Emmanuel Bernard
 */
public class Form {

	private Member member;

	@Valid
	public Member getMember() {
		return member;
	}

	public void setMember(Member m) {
		this.member = m;
	}
}
