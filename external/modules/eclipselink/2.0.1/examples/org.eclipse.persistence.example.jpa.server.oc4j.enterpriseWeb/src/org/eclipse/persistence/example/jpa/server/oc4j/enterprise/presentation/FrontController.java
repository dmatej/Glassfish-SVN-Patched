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
 *     26/02/2009 - 2.0 Michael O'Brien 
 *       - 250475: Initial example tutorial submission for OC4J 10.1.3.5 EAR
 *       - all 3 Eclipse projects required EAR, EJB and Web
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA/OC4J_Web_Tutorial
 ******************************************************************************/ 
package org.eclipse.persistence.example.jpa.server.oc4j.enterprise.presentation;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.ejb.EJB;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.eclipse.persistence.example.jpa.server.business.ApplicationServiceLocal;
import org.eclipse.persistence.example.jpa.server.business.Cell;


/**
 *   This Controller client for the stateless session bean persistence facade is used as the both the 
 *   VC in the MVC pattern.
 *   
 * Example URLs
 * http://127.0.0.1:8888/enterprise/FrontController?action=demo
 */
public class FrontController extends HttpServlet implements Servlet {
 
	@EJB(beanName="ApplicationService")
	public ApplicationServiceLocal applicationService;

	// nonJTA Application Managed EMF and EM
    private EntityManagerFactory emf  = null;
    private EntityManager entityManager = null;
    /** must match persistence.xml */
    public static final String LOCAL_PU_NAME = "exampleLocal";
    /** must match persistence.xml */
    public static final String APPLICATION_NAME= "enterprise";
    /** this must match persistence.xml - but is used for JNDI testing only */  
    public static final String JTA_DATA_SOURCE_NAME = "localJTA";


    // Servlet action strings
	public static final String EMPTY_STRING = "";
	public static final String FRONT_CONTROLLER_ACTION = "action";
	public static final String FRONT_CONTROLLER_ACTION_NONE = "none";
	public static final String FRONT_CONTROLLER_ACTION_QUERY_JPQL = "jpql";
	public static final String FRONT_CONTROLLER_ACTION_DEMO = "demo";
    public static final String FRONT_CONTROLLER_ACTION_GLIDER = "glider";	
	
	public static String exampleJPQL = "select object(e) from Cell e";

    public FrontController() {
        super();
    }
   
    private void generateGlider(PrintWriter out) {
        // Insert schema and classes into the database
        // Create glider
        // .  5  .
        // .  .  4
        // 1 2 3
        try {
        
        // Create the cells
        Cell cell1 = new Cell();
        Cell cell2 = new Cell();
        Cell cell3 = new Cell();
        Cell cell4 = new Cell();
        Cell cell5 = new Cell();        

        // Link objects in owner direction
        Set<Cell> peers1 = new HashSet<Cell>();
        Set<Cell> peers2 = new HashSet<Cell>();
        Set<Cell> peers3 = new HashSet<Cell>();
        Set<Cell> peers4 = new HashSet<Cell>();
        Set<Cell> peers5 = new HashSet<Cell>();
        peers1.add(cell2);
        
        peers2.add(cell1);
        peers2.add(cell3);
        peers2.add(cell4);
        
        peers3.add(cell2);
        peers3.add(cell4);
        
        peers4.add(cell2);
        peers4.add(cell3);
        peers4.add(cell5);        
        
        peers5.add(cell4);
        
        // Set peer cells
        cell1.setPeers(peers1);
        cell2.setPeers(peers2);
        cell3.setPeers(peers3);
        cell4.setPeers(peers4);
        cell5.setPeers(peers5);        

        // Link objects in reverse direction
        Set<Cell> refs1 = new HashSet<Cell>();
        Set<Cell> refs2 = new HashSet<Cell>();
        Set<Cell> refs3 = new HashSet<Cell>();
        Set<Cell> refs4 = new HashSet<Cell>();
        Set<Cell> refs5 = new HashSet<Cell>();
        refs1.add(cell2);
        
        refs2.add(cell1);
        refs2.add(cell3);
        refs2.add(cell4);
        
        refs3.add(cell2);
        refs3.add(cell4);
        
        refs4.add(cell2);
        refs4.add(cell3);
        refs4.add(cell5);        
        
        refs5.add(cell4);
        
        // Set peer cells
        cell1.setReferences(refs1);
        cell2.setReferences(refs2);
        cell3.setReferences(refs3);
        cell4.setReferences(refs4);
        cell5.setReferences(refs5);        

        // store first before creating relationships
        //getApplicationService().getEntityManager().persist(cell1);
        List<Cell> cells = new ArrayList<Cell>();
        cells.add(cell1);
        cells.add(cell2);
        cells.add(cell3);
        cells.add(cell4);
        cells.add(cell5);
        
        // Inserting multiple entities should be done in one transaction on the session bean
        getApplicationService().insertObjects(cells);
        // generate relationships
        //getApplicationService().getEntityManager().flush();

        } catch (Exception e) {
            out.println("Fatal error: " + e.getMessage());
            out.println("<pre>");
            e.printStackTrace(out);
            out.println("</pre>");
            e.printStackTrace();
        }        
    }
    
    
    private void processGliderCommand(HttpServletRequest request, HttpServletResponse response, PrintWriter out, String queryString) {
        generateGlider(out);
        // Get a class from the db
        List<Cell> rowsList = (List<Cell>)getApplicationService().query(queryString);
        if(null != rowsList) {
            Iterator<Cell> anIterator = rowsList.iterator();
            out.println(rowsList.size() + " Entities in storage: <br/>");
            
            // Generate an HTML table summary of all entities in the database
            out.println("<table border=\"0\">");
            int rows = 0;
            int maxColumns = 10;
            int column = maxColumns + 1;
            int objectCounter = 0;
            boolean firstRow = true;
            StringBuffer htmlRow = new StringBuffer();            
            while(anIterator.hasNext()) {
                Cell anObject = anIterator.next();
                // If we reached the end of the row - output text so far
                if(column > maxColumns) {
                    out.println(htmlRow.toString());
                    System.out.println("[EL Example]: enterprise: Object: " + anObject);
                    htmlRow = new StringBuffer();
                    if(firstRow) {
                        firstRow = false;
                    } else {
                        htmlRow.append("</tr>");
                    }
                    htmlRow.append("<tr ");
                    if(rows % 2 != 0) {
                        htmlRow.append(" bgcolor=\"#2d1d4f\">");
                    } else {
                        htmlRow.append(">");
                    }
                    column = 0;
                    rows ++;
                }
                objectCounter++;
                column++;
            
                if(null != anObject) {                        
                        htmlRow.append("<td><p class=\"refdesc-d\">");
                        htmlRow.append(objectCounter);
                        htmlRow.append(" - ");
                        htmlRow.append("<a href=\"#root\">");
                        htmlRow.append(anObject.getId().toString());
                        htmlRow.append("<img src=\"block_fff.jpg\" alt=\"");
                        htmlRow.append(anObject.toString());
                        htmlRow.append("\"/>");
                        htmlRow.append("</a></td>");
                }                
            }
            // print remaining columns of a part row
            out.print(htmlRow.toString());
            out.println("</tr></table>");
        } else {
            out.println("No rows returned or Invalid JPQL <br/>");
        }               
    }

    
    /**
     * Send servlet control to the demo command
     * @param aRequest
     * @param aResponse
     * @param out
     */
    private void processDemoCommand(HttpServletRequest aRequest, HttpServletResponse aResponse, PrintWriter out) {
        String queryString =  exampleJPQL;
        processGliderCommand(aRequest, aResponse, out, queryString);
    }
    
    /**
     * Dispatch servlet control to the command stated by the "action" URL parameter
     * @param aRequest
     * @param aResponse
     */
    private void processAction(HttpServletRequest aRequest, HttpServletResponse aResponse) {
    	// initialize the persistenceUnit if not already done
    	//getApplicationService().initialize();
		PrintWriter out = null;
		try {
			Writer writer = new BufferedWriter(new OutputStreamWriter(aResponse.getOutputStream(),"UTF-8"));
			out = new PrintWriter(writer, true);
		
				HttpSession aSession = aRequest.getSession(true);
				String action = aRequest.getParameter(FRONT_CONTROLLER_ACTION);
                if(null == action) {
                    action = FRONT_CONTROLLER_ACTION_DEMO;
                }
				printHTMLHeader(out, action);
	            // verify a JNDI lookup
	            //configureGlobalWebLogicDataSource(out);
				

				// Process requests
				if(action.equalsIgnoreCase(FRONT_CONTROLLER_ACTION_DEMO)) {
				    action = FRONT_CONTROLLER_ACTION_DEMO;
                    processDemoCommand(aRequest, aResponse, out);
                } else if(action.equalsIgnoreCase(FRONT_CONTROLLER_ACTION_GLIDER)) {
                    processGliderCommand(aRequest, aResponse, out, exampleJPQL);
                } else if(action.equalsIgnoreCase(FRONT_CONTROLLER_ACTION_DEMO)) {
                    processDemoCommand(aRequest, aResponse, out);
				}

				printHTMLFooter(out, action);
			} catch (Exception e) {
				out.println("Fatal error: " + e.getMessage());
				out.println("<pre>");
				e.printStackTrace(out);
				out.println("</pre>");
				e.printStackTrace();
			}
    }

    /**
     * Prepare response stream to write out html header/body prefix - an inside-out JSP
     * @param out
     * @param action
     */
    private void printHTMLHeader(PrintWriter out, String action) {
		out.println("<html><head><meta http-equiv=\"Content-Style-Type\" content=\"text/css\"><title>Enterprise JPA Example</title>");
		out.println("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\">");
		out.println("</head>");
		out.println("<body text=\"#ffffff\" bgcolor=\"#303030\" link=\"#33D033\" vlink=\"#D030D0\" alink=\"#D03000\">");
		out.println("<a name=\"root\"/>");
		out.println("<p class=\"ref2-d\">Action: " + action + ",   ");
		System.out.println("[EL Example]: enterprise: Action: " + action + " " + applicationService.toString());
    }

    private void printHTMLFooter(PrintWriter out, String action) {
		out.println("</body></html>");
    }
    
    /**
	 * @see Servlet#init(ServletConfig)
	 */
	public void init(ServletConfig config) throws ServletException {
		super.init();
	}

	/**
	 * @see Servlet#destroy()
	 */
	public void destroy() {
    	// close JPA
		super.destroy();
	}

	/**
	 * @see Servlet#getServletConfig()
	 */
	public ServletConfig getServletConfig() {
		return super.getServletConfig();
	}

	/**
	 * @see Servlet#getServletInfo()
	 */
	public String getServletInfo() {
		return super.getServletInfo();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		processAction(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		processAction(request, response);
	}

	/**
	 * @see HttpServlet#doPut(HttpServletRequest, HttpServletResponse)
	 */
	protected void doPut(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		//super.doPut(request, response);
		processAction(request, response);
	}

	/**
	 * Return the injected stateless session bean holding an entityManager instance
	 * @return
	 */
	public ApplicationServiceLocal getApplicationService() {
		return applicationService;
	}

	/**
	 * Return or lazy load an application managed EntityManagerFactory
	 * @return
	 */
    public EntityManagerFactory getEmf() {       
        if(null == emf) {
            emf  = Persistence.createEntityManagerFactory(LOCAL_PU_NAME);
        } 
        return emf;    
    }
    
    /**
     * Return or lazy load an application managed EntityManager
     * @return
     */
    public EntityManager getEntityManager() {        
        if(null == entityManager) {
            entityManager = getEmf().createEntityManager();
        }
        return entityManager;    
    }
	
}
