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
 *     dclarke - initial GeoNames JPA example
 ******************************************************************************/
package example.where.model;

import java.util.ArrayList;
import java.util.List;

/**
 * The User entity class represents a person using the EclipseLink JPA GeoNames
 * demo.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class User {
    private String id;
    private String firstName;
    private String lastName;
    private Location currentLocation;
    private List<Group> groups;
    private long version;

    public User() {
        this.groups = new ArrayList<Group>();
    }

    public User(String id, String firstName, String lastName) {
        this.id = id;
        this.firstName = firstName;
        this.lastName = lastName;
    }

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

    public Location getCurrentLocation() {
        return currentLocation;
    }

    public void setCurrentLocation(Location currentLocation) {
        this.currentLocation = currentLocation;
    }

    public String getId() {
        return id;
    }

    public List<Group> getGroups() {
        return groups;
    }

    public User addGroup(Group group) {
        getGroups().add(group);
        return this;
    }

    public long getVersion() {
        return version;
    }

    public String toString() {
        return "User(" + getId() + " - " + getLastName() + ", " + getFirstName() + ")";
    }
}
