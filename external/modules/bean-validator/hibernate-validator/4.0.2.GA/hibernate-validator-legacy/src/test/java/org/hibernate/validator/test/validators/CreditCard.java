//$Id: CreditCard.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.validators;

import org.hibernate.validator.EAN;

/**
 * @author Emmanuel Bernard
 */
public class CreditCard {
	@org.hibernate.validator.CreditCardNumber
	public String number;
	@EAN
	public String ean;
}
