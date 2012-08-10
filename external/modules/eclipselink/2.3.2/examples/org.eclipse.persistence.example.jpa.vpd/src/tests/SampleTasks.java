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

import model.Task;

/**
 * Simple Task population used in example test cases
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public class SampleTasks {

    public static final SampleTasks USER1_TASKS = new SampleTasks("bsmith", new Task[] { 
            new Task("Order Pizza", new Task("Tip Pizza delivery driver")), 
            new Task("Put house up for sale"), });

    public static final SampleTasks USER2_TASKS = new SampleTasks("jdoe", new Task[] { 
            new Task("Drive kids to school"), 
            new Task("Feed Fish"), 
            new Task("Pay Bills"),
            new Task("Paint house"),});

    public static final SampleTasks[] SAMPLE_TASKS = new SampleTasks[] { USER1_TASKS, USER2_TASKS };

    public static int countAllTasks() {
        int count = 0;
        for (SampleTasks st : SAMPLE_TASKS) {
            for (Task task : st.getTasks()) {
                count++;
                count += task.getNumberofSubTasks();
            }
        }
        return count;
    }

    private String userId;

    private Task[] tasks;

    public SampleTasks(String userId, Task[] tasks) {
        this.userId = userId;
        this.tasks = tasks;
    }

    public String getUserId() {
        return userId;
    }

    public Task[] getTasks() {
        return tasks;
    }

    public int countTasks() {
        int count = 0;
        for (Task task : tasks) {
            count++;
            count += task.getNumberofSubTasks();
        }
        return count;
    }
}
