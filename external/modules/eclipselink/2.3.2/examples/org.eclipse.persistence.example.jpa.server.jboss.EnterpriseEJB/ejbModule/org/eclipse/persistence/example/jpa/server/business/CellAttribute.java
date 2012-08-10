/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:     
 *     14/02/2010-2.1 Michael O'Brien 
 *       - 250477: Initial example tutorial submission for JBoss 6 EAR
 *       - all 3 Eclipse projects required EAR, EJB and Web
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA/JBoss_Web_Tutorial
 ******************************************************************************/
package org.eclipse.persistence.example.jpa.server.business;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class CellAttribute implements Serializable {
    private static final long serialVersionUID = -6399062032442498142L;
    
    @Id
    private int id;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
    
}
