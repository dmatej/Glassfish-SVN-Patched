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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.List;

import javax.persistence.EntityManager;

import model.Task;
import model.TaskStatus;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.junit.Test;

/**
 * Simple set of test cases to ensure the initial configuration of the mappings
 * is correct. In this example it assumes there is no multi-tenant configuration
 * on the {@link Task} entity and queries can see all tasks in the database.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public class InitialTests extends BaseJPATest {

    /**
     * Verify that no additional criteria or VPD policy has been configured
     */
    @Test
    public void verifyConfig() {
        ClassDescriptor desc = JpaHelper.getServerSession(getEMF()).getClassDescriptor(Task.class);

        assertNull(desc.getMultitenantPolicy());
        // This makes sure that no @AdditionalCriteria logic has been defined
        assertNull(desc.getQueryManager().getAdditionalJoinExpression());
    }

    /**
     * Read all Task instances from the database. Task instances from both users
     * will be returned.
     */
    @Test
    public void verifyReadAllTasks() {
        EntityManager em = getEMF().createEntityManager();

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t ORDER BY t.id", Task.class).getResultList();

            assertEquals(SampleTasks.countAllTasks(), tasks.size());
        } finally {
            em.close();
        }
    }

    /**
     * Run a query that will return a task for each user
     */
    @Test
    public void verifyReadByDescriptionLike() {
        EntityManager em = getEMF().createEntityManager();

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t WHERE t.description LIKE '%house%' ORDER BY t.id", Task.class).getResultList();

            assertEquals(2, tasks.size());
        } finally {
            em.close();
        }
    }

    /**
     * Run a query using native SQL. This query will return a task from each
     * user.
     */
    @Test
    public void verifyNativeReadByDescriptionLike() {
        EntityManager em = getEMF().createEntityManager();

        try {
            @SuppressWarnings("unchecked")
            List<Task> tasks = em.createNativeQuery("SELECT * FROM TASK t WHERE t.description LIKE '%house%' ORDER BY t.id").getResultList();

            assertEquals(2, tasks.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void verifyReadAllTasksForUser1() {
        EntityManager em = getEMF().createEntityManager();

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t WHERE t.user = :USER ORDER BY t.id ", Task.class).setParameter("USER", SampleTasks.USER1_TASKS.getUserId()).getResultList();

            assertEquals(SampleTasks.USER1_TASKS.countTasks(), tasks.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void verifyReadAllTasksForUser2() {
        EntityManager em = getEMF().createEntityManager();

        try {
            List<Task> tasks = em.createQuery("SELECT t FROM Task t WHERE t.user = :USER ORDER BY t.id ", Task.class).setParameter("USER", SampleTasks.USER2_TASKS.getUserId()).getResultList();

            assertEquals(SampleTasks.USER2_TASKS.countTasks(), tasks.size());
        } finally {
            em.close();
        }
    }

    @Test
    public void verifyCreateTask() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.getTransaction().begin();

            Task newUser1Task = new Task("User 1 Task");
            newUser1Task.setUser(SampleTasks.USER1_TASKS.getUserId());
            em.persist(newUser1Task);

            Task newUser2Task = new Task("User 2 Task");
            newUser1Task.setUser(SampleTasks.USER2_TASKS.getUserId());
            em.persist(newUser2Task);

            em.flush();

            int numtasks = em.createQuery("SELECT COUNT(t) FROM Task t", Number.class).getSingleResult().intValue();
            assertEquals(SampleTasks.countAllTasks() + 2, numtasks);
        } finally {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
        }
    }

    @Test
    public void updateSingleTask() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.getTransaction().begin();

            Task task = em.createNamedQuery("Task.findByDescription", Task.class).setParameter("DESC", SampleTasks.USER1_TASKS.getTasks()[0].getDescription()).getSingleResult();

            assertNotNull(task);

            task.setStatus(TaskStatus.COMPLETED);

            em.getTransaction().commit();

        } finally {
            em.close();
        }
    }

    @Test
    public void updateAllUser1Tasks() {
        EntityManager em = getEMF().createEntityManager();

        try {
            em.getTransaction().begin();

            em.createQuery("UPDATE Task t SET t.status = :STATUS WHERE t.user = :USER").setParameter("STATUS", TaskStatus.COMPLETED).setParameter("USER", SampleTasks.USER1_TASKS.getUserId()).executeUpdate();

            em.getTransaction().commit();

        } finally {
            em.close();
        }
    }

    @Override
    protected void populate() {
        EntityManager em = getEMF().createEntityManager();

        // Create Database Tables
        SchemaManager sm = new SchemaManager(em.unwrap(Server.class));
        sm.replaceDefaultTables();
        sm.replaceSequences();

        em.getTransaction().begin();

        for (SampleTasks samples : SampleTasks.SAMPLE_TASKS) {
            for (Task task : samples.getTasks()) {
                persist(em, task, samples.getUserId());
            }
        }

        em.getTransaction().commit();
        em.close();
    }

    /**
     * Simple recursive utility method that will set the user value on the task
     * and its sub tasks as well as registering the root.
     */
    private void persist(EntityManager em, Task task, String userId) {
        task.setUser(userId);
        // Handle Sub Tasks
        for (Task subtask : task.getSubtasks()) {
            persist(em, subtask, userId);
        }

        // PERSIST root task allowing cascade persist to do its work
        if (task.getParent() == null) {
            em.persist(task);
        }

    }
}
