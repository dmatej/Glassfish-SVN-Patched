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
import java.util.List;
import java.util.Map;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.QueryHint;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Multitenant;
import org.eclipse.persistence.annotations.TenantDiscriminatorColumn;
import org.eclipse.persistence.config.HintValues;
import org.eclipse.persistence.config.QueryHints;

@Entity
@Table(name = "MYS_DIV")
@Multitenant
@TenantDiscriminatorColumn(name = "LEAGUE_ID")
@NamedQueries({
    @NamedQuery(name="Division.findAll", query="SELECT d FROM Division d ORDER BY d.name",
                hints={@QueryHint(name=QueryHints.QUERY_RESULTS_CACHE, value=HintValues.TRUE)}),
    @NamedQuery(name="Division.findByName", query="SELECT d FROM Division d WHERE d.name = :NAME")
})
public class Division implements Extensible {

    @Id
    @GeneratedValue
    @Column(name = "ID")
    private int id;

    /**
     * Flag to indicate default division for use when a league has only one
     * division.
     */
    @Column(name = "DEF_DIV")
    private boolean defaultDivision = false;

    @Column(name = "NAME")
    private String name;

    @OneToMany(mappedBy = "division", cascade = { CascadeType.REMOVE })
    private List<Team> teams;

    @Version
    private long version;

    public long getVersion() {
        return version;
    }

    @Transient
    private Map<String, Object> attributes;

    public Division() {
        this.teams = new ArrayList<Team>();
    }

    public Division(String name) {
        this();
        this.name = name;
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Team> getTeams() {
        return teams;
    }

    public boolean isDefaultDivision() {
        return defaultDivision;
    }

    public void setDefaultDivision(boolean defaultDivision) {
        this.defaultDivision = defaultDivision;
    }

    @SuppressWarnings("unchecked")
    public <T> T get(String attributeName) {
        return (T) this.attributes.get(attributeName);
    }

    public Object set(String attributeName, Object value) {
        return this.attributes.put(attributeName, value);
    }

    public void addTeam(Team team) {
        getTeams().add(team);
        team.setDivision(this);

    }
}
