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
package example.mysports.service;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.StreamingOutput;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import example.mysports.model.Division;
import example.mysports.model.Divisions;
import example.mysports.model.Player;
import example.mysports.model.Team;

/**
 * Session bean exposing JAX-RS methods to allow RESTful access to the MySports
 * multi-tenant model.
 * 
 * This is a simplified REST interface for this release, which will be enhanced
 * going forward.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Stateless
@Path("/")
public class RSLeagueView {

    private LeagueRepository repository = new LeagueRepository();

    public LeagueRepository getRepository() {
        return repository;
    }

    @EJB
    public void setRepository(LeagueRepository repository) {
        this.repository = repository;
    }

    @GET
    @Path("{league}")
    @Produces(MediaType.APPLICATION_XML)
    public StreamingOutput getDivsions(@PathParam("league") String leagueId) throws JAXBException {
        getRepository().setLeagueId(leagueId);
        List<Division> divs = getRepository().getDivisions();
        return new StreamingOutputMarshaller(getRepository().getContext().getJAXBContext(), new Divisions(divs));
    }

    @GET
    @Path("{league}/{division}")
    @Produces(MediaType.APPLICATION_XML)
    public StreamingOutput getDivision(@PathParam("league") String leagueId, @PathParam("division") String division) throws JAXBException {
        getRepository().setLeagueId(leagueId);
        Division div = getRepository().getDivision(division);
        return new StreamingOutputMarshaller(getRepository().getContext().getJAXBContext(), div);
    }

    @GET
    @Path("{league}/{division}/{team}")
    @Produces(MediaType.APPLICATION_XML)
    public StreamingOutput getTeam(@PathParam("league") String leagueId, @PathParam("division") String division, @PathParam("team") String teamId) throws JAXBException {
        getRepository().setLeagueId(leagueId);
        Team team = getRepository().getTeam(division, teamId);
        return new StreamingOutputMarshaller(getRepository().getContext().getJAXBContext(), team);
    }

    @GET
    @Path("{league}/{division}/{team}/{number}")
    @Produces(MediaType.APPLICATION_XML)
    public StreamingOutput getPlayerByNumber(@PathParam("league") String leagueId, @PathParam("division") String division, @PathParam("team") String teamId, @PathParam("number") int number) throws JAXBException {
        getRepository().setLeagueId(leagueId);
        Player player = getRepository().getPlayerByNumber(division, teamId, number);
        return new StreamingOutputMarshaller(getRepository().getContext().getJAXBContext(), player);
    }

    /**
     * Simple {@link StreamingOutput} implementation that uses the provided
     * {@link JAXBContext} to marshal the result when requested. This approach
     * is uses a tenant specific {@link JAXBContext}
     */
    class StreamingOutputMarshaller implements StreamingOutput {
        private JAXBContext jaxbContext;
        private Object result;

        StreamingOutputMarshaller(JAXBContext jaxbContext, Object result) {
            this.jaxbContext = jaxbContext;
            this.result = result;
        }

        public void write(OutputStream output) throws IOException, WebApplicationException {
            if (this.jaxbContext != null && this.result != null) {
                try {
                    jaxbContext.createMarshaller().marshal(this.result, output);
                } catch (JAXBException e) {
                    throw new WebApplicationException(e, Status.INTERNAL_SERVER_ERROR);
                }
            }
        }
    }

}