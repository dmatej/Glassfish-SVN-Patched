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
package testing;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses( { CreateDatabase.class, OrderLineItemTests.class, OrderLineItemTests_IsolatedCaching.class, OrderLineItemTests_NoWeaving.class, OrderLineItemTests_IsolatedCaching_NoWeaving.class })
public class AllTests {
}
