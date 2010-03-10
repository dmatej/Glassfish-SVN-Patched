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
 *     10/02/2008-1.1M1 Michael O'Brien 
 *       - 249541: Initial example tutorial submission for WebLogic 10 EAR
 *       - all 3 Eclipse projects required EAR, EJB and Web
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA/WebLogic_Web_Tutorial
 ******************************************************************************/
package org.eclipse.persistence.example.jpa.server.business;

import java.util.List;

import javax.ejb.Local;
import javax.persistence.EntityManager;

@Local
public interface ApplicationServiceLocal {

    /**
     * Return a single object or List of objects based on a JPQL query 
     * @param jpqlQuery
     * @return
     */
    public List<Cell> query(String jpqlQuery);
    
    /**
     * persist a single object per transaction
     * @param anObject
     * @return
     */
    public boolean insert(Object anObject);

    /**
     * persist multiple entities in a single transaction
     * @param classes
     * @return
     */
    public boolean insertObjects(List<Cell> classes);
    
    /**
     * Return the injected EntityManager from this session bean
     * @return
     */
    public EntityManager getEntityManager();
}
