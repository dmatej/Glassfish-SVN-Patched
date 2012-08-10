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
package example.mysports.admin.model;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * An extension represents an additional 'virtual' attribute that an entity in
 * the MySports application can store.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Entity
@Table(name = "MYS_ADMIN_EXT")
public class Extension {

    @Id
    @GeneratedValue(generator="EXT_GEN")
    private int id;

    @ManyToOne(fetch = FetchType.LAZY)
    private HostedLeague league;

    private String entity = "";

    private String name = "";

    private String javaType = "java.lang.String";

    private String columnName;

    private String xmlPath;

    public Extension() {
    }

    public Extension(HostedLeague league, String entity, String name, String javaType, String columnName, String xmlPath) {
        setLeague(league);
        setEntity(entity);
        setName(name);
        setJavaType(javaType);
        setColumnName(columnName);
        setXmlPath(xmlPath);
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public HostedLeague getLeague() {
        return league;
    }

    public void setLeague(HostedLeague league) {
        this.league = league;
    }

    public String getEntity() {
        return entity;
    }

    public void setEntity(String entity) {
        this.entity = entity;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getJavaType() {
        return javaType;
    }

    public void setJavaType(String javaType) {
        this.javaType = javaType;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public String getXmlPath() {
        return xmlPath;
    }

    public void setXmlPath(String xmlPath) {
        this.xmlPath = xmlPath;
    }
}
