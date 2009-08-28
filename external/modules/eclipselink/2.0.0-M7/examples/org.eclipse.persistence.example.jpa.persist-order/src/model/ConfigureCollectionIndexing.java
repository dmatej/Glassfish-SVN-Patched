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
 *     dclarke - Example: Maintaining Collection Order (Bug 218321)
 *     			 http://wiki.eclipse.org/EclipseLink/Examples/JPA/Collectionordering
 *     
 *******************************************************************************/
package model;

import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.sessions.Session;

import example.CollectionIndexSessionListener;

/**
 * Example of a SessionCustomizer that configures the use of
 * CollectionIndexSessionListener to populate the collection index value of
 * Order.lineItems
 * 
 * @see example.CollectionIndexSessionListener
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class ConfigureCollectionIndexing implements SessionCustomizer {

	public void customize(Session session) throws Exception {
		CollectionIndexSessionListener listener = new CollectionIndexSessionListener();

		listener.addCollection(session, Order.class, "lineItems", "lineNumber");

		session.getEventManager().addListener(listener);
	}

}
