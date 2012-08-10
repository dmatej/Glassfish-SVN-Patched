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
package example.mysports.admin;


/**
 * League as defined on the admin server.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class League {
    private String id;
    private long version;
    private String name;
    private String colourScheme;
    private String logoUrl;
    private String uri;

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public long getVersion() {
        return version;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setVersion(long version) {
        this.version = version;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getColourScheme() {
        return colourScheme;
    }

    public void setColourScheme(String colourScheme) {
        this.colourScheme = colourScheme;
    }

    public String getLogoUrl() {
        return logoUrl;
    }

    public void setLogoUrl(String logoUrl) {
        this.logoUrl = logoUrl;
    }

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }
}
