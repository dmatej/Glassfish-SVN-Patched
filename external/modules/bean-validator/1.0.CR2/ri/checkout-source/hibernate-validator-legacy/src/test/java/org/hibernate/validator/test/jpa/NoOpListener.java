//$Id: NoOpListener.java 15133 2008-08-20 10:05:57Z hardy.ferentschik $
package org.hibernate.validator.test.jpa;

import org.hibernate.event.PreInsertEventListener;
import org.hibernate.event.PreInsertEvent;

/**
 * @author Emmanuel Bernard
 */
public class NoOpListener implements PreInsertEventListener {
	public boolean onPreInsert(PreInsertEvent event) {
		return false;
	}
}
