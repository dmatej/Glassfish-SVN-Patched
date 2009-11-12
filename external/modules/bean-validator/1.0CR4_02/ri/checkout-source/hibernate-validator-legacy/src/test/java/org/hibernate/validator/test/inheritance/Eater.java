//$Id: Eater.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator.test.inheritance;

import org.hibernate.validator.Min;

/**
 * @author Emmanuel Bernard
 */
public interface Eater {
	@Min(2)
	int getFrequency();

	void setFrequency(int frequency);
}
