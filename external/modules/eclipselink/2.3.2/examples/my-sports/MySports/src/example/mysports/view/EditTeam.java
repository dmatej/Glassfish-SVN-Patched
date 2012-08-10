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

import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.SessionScoped;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.context.FacesContext;

import example.mysports.model.Team;

@ManagedBean(name = EditTeam.NAME)
@SessionScoped
public class EditTeam extends BaseTeamBean {

    protected static final String PAGE = "edit-team?faces-redirect=true";

    protected static final String NAME = "editTeam";

    @ManagedProperty(value = "#{viewTeam}")
    private ViewTeam viewTeam;

    @Override
    public String getPage() {
        return PAGE;
    }

    @Override
    public HtmlDataTable getDataTable() {
        return getDataTable(NAME, true);
    }

    public ViewTeam getViewTeam() {
        return viewTeam;
    }

    public void setViewTeam(ViewTeam viewTeam) {
        this.viewTeam = viewTeam;
    }

    public String save() {
        FacesContext facesCtxt = FacesContext.getCurrentInstance();

        if (getTeam().getName() == null || getTeam().getName().isEmpty()) {
            facesCtxt.addMessage(null, new FacesMessage("Invalid Team Name"));
            return null;
        }

        Team team = getRepository().mergeTeam(getTeam());
        getViewTeam().setTeam(team);
        
        return ViewTeam.PAGE;
    }

    public String cancel() {
        return ViewTeam.PAGE;
    }
}
