//$Id: CreditCardNumberValidator.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator;

import java.io.Serializable;

/**
 * Check a credit card number through the Luhn algorithm
 *
 * @author Emmanuel Bernard
 */
public class CreditCardNumberValidator extends AbstractLuhnValidator implements Validator<CreditCardNumber>, Serializable {

	public void initialize(CreditCardNumber parameters) {
	}

	int multiplicator() {
		return 2;
	}
}
