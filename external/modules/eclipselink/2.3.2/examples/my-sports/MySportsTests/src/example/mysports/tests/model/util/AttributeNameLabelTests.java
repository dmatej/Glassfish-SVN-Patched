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
package example.mysports.tests.model.util;

import junit.framework.Assert;

import org.junit.Test;

import example.mysports.view.BaseTeamBean;

/**
 * Tests to verify the creation of display labels for UI from attribute names.
 * 
 * @see BaseTeamBean#label(String)
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class AttributeNameLabelTests {

    @Test
    public void test_a() {
        String label = BaseTeamBean.label("a");

        Assert.assertNotNull(label);
        Assert.assertEquals("A", label);
    }

    @Test
    public void test_A() {
        String label = BaseTeamBean.label("A");

        Assert.assertNotNull(label);
        Assert.assertEquals("A", label);
    }

    @Test
    public void test_a_() {
        String label = BaseTeamBean.label("a_");

        Assert.assertNotNull(label);
        Assert.assertEquals("A", label);
    }

    @Test
    public void test_A_() {
        String label = BaseTeamBean.label("A_");

        Assert.assertNotNull(label);
        Assert.assertEquals("A", label);
    }

    @Test
    public void test_middleName() {
        String label = BaseTeamBean.label("middleName");

        Assert.assertNotNull(label);
        Assert.assertEquals("Middle Name", label);
    }

    @Test
    public void test_MiddleName() {
        String label = BaseTeamBean.label("MiddleName");

        Assert.assertNotNull(label);
        Assert.assertEquals("Middle Name", label);
    }

}
