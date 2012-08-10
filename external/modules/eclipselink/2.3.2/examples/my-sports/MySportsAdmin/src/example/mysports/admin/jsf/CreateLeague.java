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
package example.mysports.admin.jsf;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.persistence.EntityManager;

import example.mysports.admin.ejb.PersistenceWeavingBean;
import example.mysports.admin.model.League;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean
public class CreateLeague {

    @EJB
    private PersistenceWeavingBean peristence;

    private String id;

    private String name;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    public PersistenceWeavingBean getPeristence() {
        return peristence;
    }

    public void setPeristence(PersistenceWeavingBean peristence) {
        this.peristence = peristence;
    }

    public String create() {
        EntityManager em = getPeristence().getEmf().createEntityManager();
        
        try {
            em.getTransaction().begin();
            em.persist(new League(getId(), getName()));
            em.getTransaction().commit();
        } finally {
            em.close();
        }
        
        setId("");
        setName("");
        
        return null;
    }
}
