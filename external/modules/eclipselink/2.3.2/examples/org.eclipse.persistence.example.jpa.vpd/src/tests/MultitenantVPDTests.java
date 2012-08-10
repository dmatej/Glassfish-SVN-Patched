/*******************************************************************************
 * Copyright (c) 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - Bug 356928: Multi-Tenancy with VPD Example
 ******************************************************************************/
package tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static tests.SampleTasks.USER1_TASKS;
import static tests.SampleTasks.USER2_TASKS;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import model.Task;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.MultitenantPolicy;
import org.eclipse.persistence.descriptors.SingleTableMultitenantPolicy;
import org.eclipse.persistence.descriptors.VPDMultitenantPolicy;
import org.eclipse.persistence.internal.helper.DatabaseField;
import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.Test;

/**
 * Tests used in stage 3 of tutorial to verify the @Multitenant(VPD)
 * functionality
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public class MultitenantVPDTests extends MultitenantTests {

    @Override
    @Test
    public void verifyReadAllTasks() {
        EntityManager em = getEMF().createEntityManager();

        List<Task> tasks = em.createQuery("SELECT t FROM Task t ORDER BY t.id", Task.class).getResultList();

        assertNotNull(tasks);
        assertTrue(tasks.isEmpty());
    }

    /**
     * Run a query using native SQL. There is a task for each tenant. When no
     * tenant is specified none is returned and 1 task is returned for each
     * specified tenant.
     */
    @Override
    @Test
    public void verifyNativeReadByDescriptionLike() {
        EntityManager em = getEMF().createEntityManager();

        try {
            @SuppressWarnings("unchecked")
            List<Task> tasks =  em.createNativeQuery("SELECT t.* FROM TASK t WHERE t.description LIKE '%house%' ORDER BY t.id").getResultList();
        
            assertEquals(0, tasks.size());
        } finally {
            em.close();
        }

    
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        em = getEMF().createEntityManager(properties);

        try {
            @SuppressWarnings("unchecked")
            List<Task> tasks =  em.createNativeQuery("SELECT t.* FROM TASK t WHERE t.description LIKE '%house%' ORDER BY t.id").getResultList();
        
            assertEquals(1, tasks.size());
        } finally {
            em.close();
        }

    
        properties.put("example.user-id", USER2_TASKS.getUserId());
        em = getEMF().createEntityManager(properties);

        try {
            @SuppressWarnings("unchecked")
            List<Task> tasks =  em.createNativeQuery("SELECT t.* FROM TASK t WHERE t.description LIKE '%house%' ORDER BY t.id").getResultList();
        
            assertEquals(1, tasks.size());
        } finally {
            em.close();
        }
}

    /**
     * Verify that the VPD policy is set
     */
    @Override
    @Test
    public void verifyConfig() {
        ClassDescriptor desc = JpaHelper.getServerSession(getEMF()).getClassDescriptor(Task.class);

        // Verify MultitenantPolicy being used
        MultitenantPolicy policy = desc.getMultitenantPolicy();
        assertNotNull("No MultitenantPolicy configured", policy);
        assertFalse(policy.getClass().equals(SingleTableMultitenantPolicy.class));
        assertTrue(policy.getClass().equals(VPDMultitenantPolicy.class));

        // Verify VPDMultitenantPolicy
        VPDMultitenantPolicy vpdPolicy = (VPDMultitenantPolicy) policy;

        assertTrue("No tenant discriminator columns specified", vpdPolicy.hasTenantDiscriminatorFields());
        assertEquals("Only 1 discriminator column expected", 1, vpdPolicy.getTenantDiscriminatorFields().size());

        DatabaseField field = vpdPolicy.getTenantDiscriminatorFields().keySet().iterator().next();
        assertEquals("Inncorrect discrimintaor column", "USER_ID", field.getName());

        String contextPropertyName = vpdPolicy.getTenantDiscriminatorFields().get(field);
        assertEquals("Incorrect multitenant context property", "example.user-id", contextPropertyName);
    }

}
