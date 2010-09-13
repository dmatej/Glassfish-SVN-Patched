//$Id: Version.java 15169 2008-09-10 10:53:41Z hardy.ferentschik $
package org.hibernate.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Hibernate Vaildator version
 *
 * @author Emmanuel Bernard
 */
public class Version {
	public static final String VERSION = "3.1.0.GA";
	private static Logger log = LoggerFactory.getLogger( Version.class );

	static {
		log.info( "Hibernate Validator {}", VERSION );
	}

	public static void touch() {
	}
}