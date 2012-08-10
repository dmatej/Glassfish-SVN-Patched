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
package example.mysports.view;

import javax.ejb.EJB;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;

import example.mysports.model.Division;
import example.mysports.service.LeagueRepository;

/**
 * Managed JSF bean used to create a new {@link Division}
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean
@RequestScoped
public class CreateDivision {

    @EJB
    private LeagueRepository repository;

    private String name;

    public LeagueRepository getRepository() {
        return this.repository;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String create() {
        FacesContext facesCtxt = FacesContext.getCurrentInstance();

        if (getName() == null || getName().isEmpty()) {
            facesCtxt.addMessage(null, new FacesMessage("Invalid Name"));
            return null;
        }
 
        getRepository().addDivision(getName());
        return cancel();
    }

    public String cancel() {
        return ViewLeague.PAGE + "?faces-redirect=true";
    }
}
