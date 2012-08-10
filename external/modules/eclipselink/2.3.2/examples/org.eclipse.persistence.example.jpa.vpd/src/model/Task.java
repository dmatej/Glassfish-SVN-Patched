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
 * 		dclarke - Bug 356928: Multi-Tenancy with VPD Example
 ******************************************************************************/
package model;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Version;

/**
 * Simple domain object which represents a task for a given application user.
 * The tasks are stored in a shared multi-tenant table with the rows
 * discriminated by the USER_ID column's value. Each user can see it's own list of tasks. Each task
 * has a set of subtasks.
 * 
 * @author pkrogh, dclarke
 * @since EclipseLink 2.3.1
 */
@Entity
@NamedQuery(name = "Task.findByDescription", query = "SELECT t FROM Task t WHERE t.description = :DESC")
public class Task {

    @Id
    @GeneratedValue
    private int id;

    private String description;

    @Enumerated(EnumType.STRING)
    private TaskStatus status;

    @OneToMany(mappedBy = "parent", cascade = CascadeType.ALL)
    private List<Task> subtasks = new ArrayList<Task>();

    @OneToOne(fetch = FetchType.LAZY)
    private Task parent;

    @Column(name = "USER_ID")
    private String user;

    @Version
    private long version;

    public Task() {
    }

    public Task(String description, Task... subtasks) {
        this.description = description;
        this.status = TaskStatus.CREATED;

        for (Task task : subtasks) {
            addSubtask(task);
        }
    }

    public int getId() {
        return id;
    }

    public void setId(int empId) {
        this.id = empId;
    }

    public Task getParent() {
        return parent;
    }

    public void setParent(Task parent) {
        this.parent = parent;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public TaskStatus getStatus() {
        return this.status;
    }

    public List<Task> getSubtasks() {
        return subtasks;
    }

    public void setStatus(TaskStatus status) {
        this.status = status;
    }

    public void addSubtask(Task task) {
        task.parent = this;
        this.subtasks.add(task);
    }

    public String getUser() {
        return this.user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public long getVersion() {
        return version;
    }

    public int getNumberofSubTasks() {
        int count = 0;
        for (Task task : getSubtasks()) {
            count += 1 + task.getNumberofSubTasks();
        }
        return count;
    }

    public String toString() {
        return "Task(id: " + getId() + " -- " + getDescription() + " - " + getStatus() + ")";
    }

}
