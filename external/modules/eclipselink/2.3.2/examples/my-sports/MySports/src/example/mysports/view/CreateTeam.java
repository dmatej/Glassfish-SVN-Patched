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

import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.context.FacesContext;

import example.mysports.model.Player;
import example.mysports.model.Team;

/**
 * JSF managed bean used to create a new {@link Team}. An empty {@link Team}
 * instance is created and used as the target in the {@link BaseTeamBean} with a
 * default set of empty {@link Player}
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean(name = CreateTeam.NAME)
@SessionScoped
public class CreateTeam extends BaseTeamBean {

    protected static final String NAME = "createTeam";

    protected static final String PAGE = "create-team?faces-redirect=true";

    @Override
    public String getPage() {
        return PAGE;
    }

    @Override
    public HtmlDataTable getDataTable() {
        return getDataTable(NAME, true);
    }

    @Override
    public String setTeam() {
        Team team = new Team();
        team.setName("");
        team.setDivision(getRepository().getCurrentDivision());
        team.getPlayers().clear();
        for (int index = 0; index < 12; index++) {
            team.addPlayer(new Player());
        }
        setTeam(team);
        return getPage();
    }

    public String create() {
        return setTeam();
    }

    public String save() {
        FacesContext facesCtxt = FacesContext.getCurrentInstance();

        if (getTeam().getName() == null || getTeam().getName().isEmpty()) {
            facesCtxt.addMessage(null, new FacesMessage("Invalid team Name"));
        }

        List<Player> validPlayers = getValidPlayers();
        if (validPlayers.isEmpty()) {
            facesCtxt.addMessage(null, new FacesMessage("Must have at least one player"));
        }

        if (!facesCtxt.getMessageList().isEmpty()) {
            return null;
        }

        getRepository().addTeam(getTeam().getName(), getValidPlayers(), null);
        return ViewDivision.PAGE;
    }

    /**
     * @return a list of players who have numbers and names
     */
    private List<Player> getValidPlayers() {
        List<Player> validPlayers = new ArrayList<Player>();
        for (Player player : getTeam().getPlayers()) {
            if (player.isValid()) {
                validPlayers.add(player);
            }
        }
        return validPlayers;
    }

}
