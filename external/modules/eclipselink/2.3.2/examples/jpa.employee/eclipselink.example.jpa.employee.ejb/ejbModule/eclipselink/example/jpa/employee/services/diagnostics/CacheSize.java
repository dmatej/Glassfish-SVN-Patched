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
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services.diagnostics;

/**
 * Simple utility that returns the current state of an EclipseLink Cache.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class CacheSize {
    private String type;
    private int size;

    public CacheSize(String typeName, int size) {
        this.type = typeName;
        this.size = size;
    }

    public String getType() {
        return type;
    }

    public Integer getSize() {
        return size;
    }

}
