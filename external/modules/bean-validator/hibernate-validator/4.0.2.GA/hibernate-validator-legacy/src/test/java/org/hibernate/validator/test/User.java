//$Id: User.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator.test;

import org.hibernate.validator.Email;
import org.hibernate.validator.NotNull;

/**
 * @author Emmanuel Bernard
 */
public class User {
	@NotNull
	public String name;
	@Email
	public String email;
}
