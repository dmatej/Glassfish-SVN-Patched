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
import static org.junit.Assert.fail;
import static tests.SampleTasks.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import model.Task;
import model.TaskStatus;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.MultitenantPolicy;
import org.eclipse.persistence.descriptors.SingleTableMultitenantPolicy;
import org.eclipse.persistence.descriptors.VPDMultitenantPolicy;
import org.eclipse.persistence.exceptions.QueryException;
import org.eclipse.persistence.internal.helper.DatabaseField;
import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.Test;

public class MultitenantTests extends BaseJPATest {

    @Test
    public void verifyReadAllTasksForUser1() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t ORDER BY t.id ", Task.class).getResultList();

            assertEquals(SampleTasks.USER1_TASKS.countTasks(), tasks.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void verifyReadAllTasksForUser2() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER2_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t ORDER BY t.id ", Task.class).getResultList();

            assertEquals(SampleTasks.USER2_TASKS.countTasks(), tasks.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void verifyReadAllTasks() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.createQuery("SELECT t FROM Task t ORDER BY t.id", Task.class).getResultList();
        } catch (QueryException e) {
            return;
        } finally {
            em.close();
        }
        fail("QueryException not thrown for missing context property value");
    }

    /**
     * Run a query that will return a task for each user. Since the query is
     * tenant limited on 1 will be returned.
     */
    @Test
    public void verifyReadByDescriptionLike() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t WHERE t.description LIKE '%house%' ORDER BY t.id", Task.class).getResultList();

            assertEquals(1, tasks.size());
        } finally {
            em.close();
        }
    }

    /**
     * Run a query using native SQL. This query would return a task from each
     * user but since native SQL is disabled (by default) their query will not
     * be allowed to execute.
     */
    @Test
    public void verifyNativeReadByDescriptionLike() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.createNativeQuery("SELECT t FROM TASK t WHERE t.description LIKE '%house%' ORDER BY t.id").getResultList();
        } catch (QueryException qe) {
            return;
        } finally {
            em.close();
        }
        fail("QueryException not thrown for native SQL usage");
    }

    @Test
    public void verifyCreateTask_User1() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            em.getTransaction().begin();

            Task newUser1Task = new Task("User 1 Task");
            em.persist(newUser1Task);

            em.flush();

            int numtasks = em.createQuery("SELECT COUNT(t) FROM Task t", Number.class).getSingleResult().intValue();
            assertEquals(SampleTasks.USER1_TASKS.countTasks() + 1, numtasks);
        } finally {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
        }
    }

    @Test
    public void updateSingleTask() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            em.getTransaction().begin();

            Task task = em.createNamedQuery("Task.findByDescription", Task.class).setParameter("DESC", USER1_TASKS.getTasks()[0].getDescription()).getSingleResult();

            assertNotNull(task);

            task.setStatus(TaskStatus.COMPLETED);

            em.getTransaction().commit();

        } finally {
            em.close();
        }
    }

    @Test
    public void updateAllUser1Tasks() {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager em = getEMF().createEntityManager(properties);

        try {
            em.getTransaction().begin();

            em.createQuery("UPDATE Task t SET t.status = :STATUS").setParameter("STATUS", TaskStatus.COMPLETED).executeUpdate();

            em.getTransaction().commit();

        } finally {
            em.close();
        }
    }

    /**
     * Verify that the Task class and the persistence unit are configured for
     * 
     * @Multitenant(SINGLE_TABLE) usage.
     */
    @Test
    public void verifyConfig() {
        ClassDescriptor desc = JpaHelper.getServerSession(getEMF()).getClassDescriptor(Task.class);

        // Tenant Discriminator columns
        MultitenantPolicy policy = desc.getMultitenantPolicy();
        assertNotNull("No MultitenantPolicy configured", policy);
        assertTrue(policy instanceof SingleTableMultitenantPolicy);
        assertFalse(policy instanceof VPDMultitenantPolicy);

        SingleTableMultitenantPolicy stPolicy = (SingleTableMultitenantPolicy) policy;
        assertTrue("No tenant discriminator columns specified", stPolicy.hasTenantDiscriminatorFields());
        assertEquals("Only 1 discriminator column expected", 1, stPolicy.getTenantDiscriminatorFields().size());

        DatabaseField field = stPolicy.getTenantDiscriminatorFields().keySet().iterator().next();
        assertEquals("Inncorrect discrimintaor column", "USER_ID", field.getName());

        String contextPropertyName = stPolicy.getTenantDiscriminatorFields().get(field);
        assertEquals("Incorrect multitenant context property", "example.user-id", contextPropertyName);
    }

    @Override
    protected void populate() {

        // USER 1
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("example.user-id", USER1_TASKS.getUserId());
        EntityManager emU1 = getEMF().createEntityManager(properties);

        emU1.getTransaction().begin();
        for (Task task : SampleTasks.USER1_TASKS.getTasks()) {
            emU1.persist(task);
        }
        emU1.getTransaction().commit();
        emU1.close();

        // USER 2
        properties.put("example.user-id", USER2_TASKS.getUserId());
        EntityManager emU2 = getEMF().createEntityManager(properties);

        emU2.getTransaction().begin();
        for (Task task : SampleTasks.USER2_TASKS.getTasks()) {
            emU2.persist(task);
        }
        emU2.getTransaction().commit();
        emU2.close();
    }

}
