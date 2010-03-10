/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Bug 277731: Simple Map Dynamic JPA Example
 *               http://wiki.eclipse.org/EclipseLink/Examples/JPA/Dynamic/SimpleDynamicMap
 ******************************************************************************/
package testing;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import testing.jpa.DynamicMapWithRelationships_Tests;

@RunWith(Suite.class)
@Suite.SuiteClasses( { testing.nativeorm.DynamicMap_Tests.class, 
                       testing.jpa.DynamicMap_Tests.class, 
                       DynamicMapWithRelationships_Tests.class })
public class AllTests {

}
