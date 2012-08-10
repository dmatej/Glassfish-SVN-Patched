/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - TODO
 ******************************************************************************/
package eclipselink.example.jpa.employee.model;

import java.sql.Timestamp;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * Member of a Project relating an {@link Employee} and a {@link Project} with
 * temporal information about when they joined and when they leave a project.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
@Entity
@Table(name = "PROJ_MEMBER")
@Cacheable(false)
public class Member {

    @Id
    @GeneratedValue
    @Column(name = "MEM_ID")
    private int id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "PROJ_ID")
    private Project project;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "EMP_ID")
    private Employee employee;

    /**
     * The point in time where the {@link #employee} joined the {@link #project}
     * .
     */
    @Column(name = "START_DATE")
    private Timestamp start;

    /**
     * The point in time where the {@link #employee} left the project or changed
     * their leadership role. A NULL value indicates that the employee is still
     * a member of the {@link #project}
     */
    @Column(name = "END_DATE")
    private Timestamp end;

    private Member() {
    }

    public Member(Employee emp, Project proj, Timestamp asOf, boolean isLead) {
        this();

        this.employee = emp;
        this.project = proj;
        this.start = asOf;
        this.lead = isLead;
    }

    /**
     * Indicates if the member employee is the lead of the project. Changing the
     * lead of a project should involve ending and starting a new membership.
     */
    @Column(name = "IS_LEAD")
    private boolean lead = false;

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public Project getProject() {
        return project;
    }

    public Employee getEmployee() {
        return employee;
    }

    public Timestamp getStart() {
        return start;
    }

    public Timestamp getEnd() {
        return end;
    }

    protected void setEnd(Timestamp end) {
        this.end = end;
    }

    public boolean isLead() {
        return lead;
    }

    /**
     * Determine if this member should be part of the current membership
     * collection for the provided time.
     */
    public boolean isCurrent(Timestamp ts) {
        int startCompare = getStart().compareTo(ts);
        if (startCompare <= 0) {
            if (getEnd() != null) {
                int endCompare = getEnd().compareTo(ts);
                if (endCompare <= 0) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * TODO
     * 
     * @return
     */
    public Member removeAslead(Timestamp asOfTS) {
        if (!isLead() || getEnd() != null) {
            throw new IllegalStateException("TODO");
        }
        setEnd(asOfTS);
        return new Member(getEmployee(), getProject(), asOfTS, false);
    }
}
