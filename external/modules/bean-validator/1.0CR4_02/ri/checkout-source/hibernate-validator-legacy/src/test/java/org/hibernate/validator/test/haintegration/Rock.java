//$Id: Rock.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.haintegration;

import javax.persistence.Entity;

import org.hibernate.validator.NotNull;

/**
 * @author Emmanuel Bernard
 */
@Entity
public class Rock extends Music {
	@NotNull
	public Integer bit;
}
