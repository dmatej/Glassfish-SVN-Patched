package model.spatial.simple;

import oracle.spatial.geometry.JGeometry;

/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved. This program and the
 * accompanying materials are made available under the terms of the Eclipse
 * Public License v1.0 and Eclipse Distribution License v. 1.0 which accompanies
 * this distribution. The Eclipse Public License is available at
 * http://www.eclipse.org/legal/epl-v10.html and the Eclipse Distribution
 * License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 * 
 * Contributors: dclarke - Oracle Spatial Example (Bug 211007) Initial
 * Contribution
 ******************************************************************************/

public class SimpleSpatial {
    private long id;
    private JGeometry geometry;
    private String name;

    public SimpleSpatial() {
    }

    public SimpleSpatial(long id, JGeometry geometry, String name) {
        this.id = id;
        this.geometry = geometry;
        this.name = name;
    }

    public SimpleSpatial(long id, JGeometry geometry) {
        this(id, geometry, null);
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public void setGeometry(JGeometry geometry) {
        this.geometry = geometry;
    }

    public JGeometry getGeometry() {
        return geometry;
    }

    public String toString() {
        return "SimpleSpatial(" + getId() + ", " + getGeometry() + "))";
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
