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

import java.util.List;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;

import example.mysports.model.Division;
import example.mysports.service.LeagueRepository;

/**
 * JSF managed bean used to view a league's divisions.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean
public class ViewLeague {

    protected static final String PAGE = "view-league?faces-redirect=true";

    @EJB
    private LeagueRepository repository;

    public LeagueRepository getRepository() {
        return this.repository;
    }

    public List<Division> getDivisions() {
        return getRepository().getDivisions();
    }

    public Division getCurrentDivision() {
        return getRepository().getCurrentDivision();
    }

    public String view() {
        return PAGE;
    }
}
