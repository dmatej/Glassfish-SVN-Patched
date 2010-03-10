/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package example.where.persistence;

import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.mappings.converters.Converter;
import org.eclipse.persistence.sessions.Session;

/**
 * Convert between a comma separated database value with an array of Strings in Java
 *  
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class StringArrayConverter implements Converter{

	public Object convertDataValueToObjectValue(Object arg0, Session arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	public Object convertObjectValueToDataValue(Object arg0, Session arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	public void initialize(DatabaseMapping arg0, Session arg1) {
		// TODO Auto-generated method stub
		
	}

	public boolean isMutable() {
		// TODO Auto-generated method stub
		return false;
	}

}
