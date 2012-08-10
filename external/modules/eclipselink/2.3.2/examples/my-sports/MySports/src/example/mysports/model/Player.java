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
package example.mysports.model;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Multitenant;
import org.eclipse.persistence.annotations.TenantDiscriminatorColumn;
import org.eclipse.persistence.annotations.VirtualAccessMethods;

/**
 * In the MySports demo a Player entity represents an individual member of a
 * team.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Entity
@Table(name = "MYS_PLAYER")
@Multitenant
@TenantDiscriminatorColumn(name = "LEAGUE_ID")
@VirtualAccessMethods
public class Player implements Extensible {

    @Id
    @GeneratedValue
    @Column(name = "ID")
    private int id;

    @Column(name = "USER_ID")
    private String userid;

    @Column(name = "F_NAME")
    private String firstName;

    @Column(name = "L_NAME")
    private String lastName;

    @Column(name = "EMAIL")
    private String email;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "TEAM_ID")
    private Team team;

    @Column(name = "NUM")
    private int number;

    @Version
    private long version;

    /**
     * Extended attributes
     */
    @Transient
    private Map<String, Object> attributes = new HashMap<String, Object>();

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public Team getTeam() {
        return team;
    }

    public void setTeam(Team team) {
        this.team = team;
    }

    public int getNumber() {
        return number;
    }

    public void setNumber(int number) {
        this.number = number;
    }

    public int getId() {
        return id;
    }

    public String getUserid() {
        return userid;
    }

    public void setUserid(String userid) {
        this.userid = userid;
    }

    public long getVersion() {
        return version;
    }

    @SuppressWarnings("unchecked")
    public <T> T get(String attributeName) {
        return (T) this.attributes.get(attributeName);
    }

    public Object set(String attributeName, Object value) {
        return this.attributes.put(attributeName, value);
    }

    /**
     * Determine if player instance is valid to be added to a team. This is true
     * if the player has a number and both names.
     * 
     * @return
     */
    public boolean isValid() {
        if (getNumber() <= 0) {
            return false;
        }
        if (getFirstName() == null || getFirstName().isEmpty()) {
            return false;
        }
        if (getLastName() == null || getLastName().isEmpty()) {
            return false;
        }
        return true;
    }

}
