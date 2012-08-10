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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.ejb.EJB;
import javax.el.ValueExpression;
import javax.faces.application.FacesMessage;
import javax.faces.component.html.HtmlColumn;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.persistence.metamodel.Attribute;
import javax.servlet.http.HttpServletRequest;

import example.mysports.model.Division;
import example.mysports.model.Player;
import example.mysports.model.Team;
import example.mysports.service.LeagueRepository;

/**
 * Base managed bean for Team operations.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public abstract class BaseTeamBean {

    @EJB
    private LeagueRepository repository;

    private Team team;

    private List<ExtensibleEntity<Player>> players;

    private HtmlDataTable dataTable;

    public LeagueRepository getRepository() {
        return this.repository;
    }

    /**
     * Default page that will be navigated to on successful call to
     * {@link #setTeam())}
     */
    public abstract String getPage();

    public String getPage(String pageName) {
        return pageName + "?faces-redirect=true";
    }

    public Division getCurrentDivision() {
        return getRepository().getCurrentDivision();
    }

    public Team getTeam() {
        return this.team;
    }

    protected void setTeam(Team team) {
        this.team = team;
        this.dataTable = null;

        this.players = new ArrayList<ExtensibleEntity<Player>>();
        for (Player player : getTeam().getPlayers()) {
            this.players.add(new ExtensibleEntity<Player>(player));
        }
    }

    public String setTeam() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) facesContext.getExternalContext().getRequest();
        String value = myRequest.getParameter("team-id");
        if (value != null && !value.isEmpty()) {
            Team newTeam = getRepository().find(Team.class, Integer.valueOf(value));
            if (newTeam == null) {
                facesContext.addMessage(null, new FacesMessage("No Team found for id: " + value));
                return null;
            }
            setTeam(newTeam);
        }

        return getPage();
    }

    public List<ExtensibleEntity<Player>> getPlayers() {
        return this.players;
    }

    public abstract HtmlDataTable getDataTable();

    /**
     * Construct a data table for {@link Player} including its extended
     * attributes.
     * 
     * @see LeagueRepository#getAdditionalAttributes(Class)
     */
    public HtmlDataTable getDataTable(String beanName, boolean writeable) {
        if (this.dataTable == null) {
            HtmlDataTable table = new HtmlDataTable();
            table.setValueExpression("value", createValueExpression("#{" + beanName + ".players}", List.class));
            table.setVar("player");
            table.setWidth("100%");
            table.setBorder(1);
            table.setStyleClass("pretty");

            addColumn(table, "Number", "#{player.entity.number}", Integer.class, writeable);
            addColumn(table, "First Name", "#{player.entity.firstName}", String.class, writeable);
            addColumn(table, "Last Name", "#{player.entity.lastName}", String.class, writeable);

            for (Attribute<?, ?> attr : getRepository().getAdditionalAttributes(Player.class)) {
                addColumn(table, label(attr.getName()), "#{player.get('" + attr.getName() + "').value}", attr.getJavaType(), writeable);
            }

            this.dataTable = table;
        }
        return this.dataTable;
    }

    public void setDataTable(HtmlDataTable dataTable) {
        this.dataTable = dataTable;
    }

    /**
     * Helper method to add a column to the {@link #dataTable}
     */
    private void addColumn(HtmlDataTable table, String name, String valueExpression, Class<?> valueType, boolean writeable) {
        HtmlColumn column = new HtmlColumn();
        table.getChildren().add(column);

        HtmlOutputText header = new HtmlOutputText();
        header.setValue(name);
        column.setHeader(header);

        if (writeable) {
            HtmlInputText input = new HtmlInputText();
            input.setValueExpression("value", createValueExpression(valueExpression, valueType));
            column.getChildren().add(input);
        } else {
            HtmlOutputText output = new HtmlOutputText();
            output.setValueExpression("value", createValueExpression(valueExpression, valueType));
            column.getChildren().add(output);
        }
    }

    private ValueExpression createValueExpression(String valueExpression, Class<?> valueType) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        return facesContext.getApplication().getExpressionFactory().createValueExpression(facesContext.getELContext(), valueExpression, valueType);
    }

    /**
     * Convert attributeName into String name based on Camel casing
     */
    public static String label(String attrName) {
        if (attrName == null || attrName.isEmpty()) {
            return attrName;
        }

        String name = attrName.substring(0, 1).toUpperCase() + attrName.substring(1);

        String[] lowerSegements = name.split("[A-Z_]");
        String[] upperStrings = name.split("[a-z_]");

        StringWriter writer = new StringWriter();
        int upperIndex = 0;
        int lowerIndex = 0;

        writer.write(upperStrings[upperIndex++]);

        while (lowerIndex < lowerSegements.length) {
            if (lowerSegements[lowerIndex].isEmpty()) {
                lowerIndex++;
            } else {
                writer.write(lowerSegements[lowerIndex++]);
                writer.write(" ");

                while (upperIndex < upperStrings.length && upperStrings[upperIndex].isEmpty()) {
                    upperIndex++;
                }
                if (upperIndex < upperStrings.length) {
                    writer.write(upperStrings[upperIndex++]);
                }
            }
        }

        return writer.toString().trim();
    }
}
