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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Multitenant;
import org.eclipse.persistence.annotations.TenantDiscriminatorColumn;

@Entity
@Table(name = "MYS_TEAM")
@Multitenant
@TenantDiscriminatorColumn(name = "LEAGUE_ID")
@NamedQuery(name="Team.findByDivisionAndName", query="SELECT t FROM Team t WHERE t.name = :NAME AND t.division.name = :DIV")
public class Team implements Extensible {

    @Id
    @GeneratedValue
    @Column(name = "ID")
    private int id;

    @Column(name = "NAME")
    private String name;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "DIVISION_ID")
    private Division division;

    @OneToMany(mappedBy = "team", cascade = CascadeType.ALL)
    @OrderBy("number ASC")
    private List<Player> players;

    @Version
    private long version;
    
    @Transient
    private Map<String, Object> attributes;

    public Team() {
        this.players = new ArrayList<Player>();
        this.attributes = new HashMap<String, Object>();
    }

    public Team(String name) {
        this();
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Division getDivision() {
        return division;
    }

    public void setDivision(Division division) {
        this.division = division;
    }

    public int getId() {
        return id;
    }

    public List<Player> getPlayers() {
        return players;
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

    public void addPlayer(Player player) {
        getPlayers().add(player);
        player.setTeam(this);
    }

}
