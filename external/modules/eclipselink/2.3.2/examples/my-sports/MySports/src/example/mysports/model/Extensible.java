/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.model;

import example.mysports.view.ExtensibleEntity;

/**
 * Application specific marker interface for an extensible type. Not required for EclipseLink JPA.
 * 
 * @see ExtensibleEntity for usage in JSF 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public interface Extensible {

    public <T> T get(String attributeName);
    
    public Object set(String attributeName, Object value);
}
