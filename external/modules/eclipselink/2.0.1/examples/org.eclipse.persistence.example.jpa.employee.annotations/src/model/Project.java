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
package model;

import java.io.Serializable;

import javax.persistence.*;
import static javax.persistence.InheritanceType.JOINED;
import static javax.persistence.FetchType.LAZY;

/**
 *
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Inheritance(strategy = JOINED)
@DiscriminatorColumn(name = "PROJ_TYPE")
public abstract class Project implements Serializable {
    @Id
    @Column(name = "PROJ_ID")
    @GeneratedValue
    private int id;
    @Basic
    @Column(name="PROJ_NAME")
    private String name;
    @Basic
    @Column(name="DESCRIP") 
    private String description;
    @Version
    private Long version;
    @ManyToOne(fetch=LAZY)
    @JoinColumn(name="LEADER_ID")
    private Employee teamLeader;

    public Project() {
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

    public Long getVersion() {
        return version;
    }

    public void setVersion(Long version) {
        this.version = version;
    }

    public Employee getTeamLeader() {
        return this.teamLeader;
    }

    public void setTeamLeader(Employee employee) {
        this.teamLeader = employee;
    }
}
