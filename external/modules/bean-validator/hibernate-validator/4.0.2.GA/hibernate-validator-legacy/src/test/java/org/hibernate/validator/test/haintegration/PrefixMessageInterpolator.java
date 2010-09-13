//$Id: PrefixMessageInterpolator.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.haintegration;

import org.hibernate.validator.MessageInterpolator;
import org.hibernate.validator.Validator;

/**
 * @author Emmanuel Bernard
 */
public class PrefixMessageInterpolator implements MessageInterpolator {
	public String interpolate(String message, Validator validator, MessageInterpolator defaultInterpolator) {
		return "prefix_" + defaultInterpolator.interpolate( message, validator, defaultInterpolator );
	}
}
