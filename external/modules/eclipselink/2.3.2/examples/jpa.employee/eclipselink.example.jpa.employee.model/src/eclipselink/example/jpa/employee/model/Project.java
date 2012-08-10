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
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 *              mbraeuer - annotated version
 ******************************************************************************/
package eclipselink.example.jpa.employee.model;

import static javax.persistence.InheritanceType.JOINED;

import java.sql.Date;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Cacheable;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.OneToMany;
import javax.persistence.Version;

/**
 * 
 * <p>
 * Caching: Since the Project has a collection of member that depends on the
 * context of the time being selected it will not be cached. In order to put it
 * in the shared cache the {@link #members} relationship would need to removed
 * or made not temporal.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Inheritance(strategy = JOINED)
@DiscriminatorColumn(name = "PROJ_TYPE")
@Cacheable(false)
public abstract class Project {

    @Id
    @Column(name = "PROJ_ID")
    @GeneratedValue(strategy = GenerationType.TABLE)
    private int id;

    @Column(name = "PROJ_NAME")
    private String name;

    @Column(name = "DESCRIP")
    private String description;

    @Column(name = "START_DATE")
    private Date startDate;

    @Column(name = "END_DATE")
    private Date endDate;

    @Version
    private Long version;

    /**
     * Member of the project. This collection is temporal in nature based on the
     * {@link Member}'s start and end dates. The {@link EntityManager} will
     * require a point in time configured on it to limit the members returned
     * when queried. *
     * <p>
     * The members relationships is treated as primarily owned by the project so
     * all cascading is done from this side
     * 
     * TODO-dclarke: Add @see for temporal filtering utility
     */
    @OneToMany(mappedBy = "project", cascade = CascadeType.ALL)
    private List<Member> members;

    public Project() {
        this.members = new ArrayList<Member>();
    }

    public String getDescription() {
        return this.description;
    }

    public void setDescription(String descrip) {
        this.description = descrip;
    }

    public int getId() {
        return this.id;
    }

    public void setId(int projId) {
        this.id = projId;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String projName) {
        this.name = projName;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * Return a String for the type of project. This is used in user interface
     * and is not defined by the JPA mappings.
     */
    public abstract String getType();

    public Long getVersion() {
        return version;
    }

    public void setVersion(Long version) {
        this.version = version;
    }

    /**
     * TODO Temporal list of members as of the current time specified in the
     * {@link EntityManager} properties.
     * 
     * @return
     */
    public List<Member> getMembers() {
        return members;
    }

    public Member getLead() {
        for (Member member : getMembers()) {
            if (member.isLead()) {
                return member;
            }
        }
        return null;
    }

    /**
     * TODO
     * 
     * @param employee
     * @return
     */
    public Member getMember(Employee employee) {
        for (Member member : getMembers()) {
            if (member.getEmployee().equals(employee)) {
                return member;
            }
        }
        return null;
    }

    /**
     * 
     * @param employee
     * @param asOf
     * @param isLead
     * @param currentTS
     * 
     * @return the collection of new and modified memberships caused by adding
     *         this. The last one is always the newly created one.
     */
    public List<Member> addMember(Employee employee, Timestamp asOfTS, boolean isLead, Timestamp currentTS) {
        List<Member> newMemberships = new ArrayList<Member>();

        if (isLead) {
            Member lead = getLead();
            if (lead != null) {
                Member prevLeadNewMember = lead.removeAslead(asOfTS);
                newMemberships.add(prevLeadNewMember);
                if (prevLeadNewMember.isCurrent(currentTS)) {
                    getMembers().add(prevLeadNewMember);
                    getMembers().remove(lead);
                }
            }
        }

        Member member = new Member(employee, this, asOfTS, isLead);
        newMemberships.add(member);
        if (member.isCurrent(currentTS)) {
            getMembers().add(member);
        }

        return newMemberships;
    }

    /**
     * TODO
     * 
     * @param employee
     * @return
     */
    public Member removeAsMember(Employee employee, Timestamp asOf, Timestamp current) {
        Member member = getMember(employee);
        
        if (member != null) {
            member.setEnd(asOf);
            
            if (!member.isCurrent(current)) {
                getMembers().remove(member);
            }
        }
        
        return member;
    }

    public String toString() {
        return getType() + "(" + getId() + " - " + getName() + ")";
    }
}
