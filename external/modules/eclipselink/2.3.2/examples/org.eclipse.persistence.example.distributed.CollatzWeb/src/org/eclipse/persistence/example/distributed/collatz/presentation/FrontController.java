/*******************************************************************************
 * Copyright (c) 2010, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     10/03/2011 2.3  Michael O'Brien 
 *          - 337037: initial API and implementation platform to be used for 
 *             distributed EE application research, development and architecture
 ******************************************************************************/  
package org.eclipse.persistence.example.distributed.collatz.presentation;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;

import javax.ejb.EJB;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
//import javax.servlet.http.HttpSession;

import org.eclipse.persistence.example.distributed.collatz.business.CollatzFacadeLocal;

/**
 * <pattern>FrontController</pattern><br>
 * This class is the controller end of an active client first principles ajaxclient.jsp.
 * Normally a standard JSF .xhtml and @ManagedBean presentation bean would be used.
 * It is part of a distributed application framework used to simulate and research
 * concurrency, analytics, management, performance and exception handling.
 * The focus is on utilizing JPA 2.0 as the persistence layer for scenarios involving
 * multicore, multithreaded and multiuser distributed memory L1 persistence applications.
 * The secondary focus is on exercising Java EE6 API to access the results of this distributed application.
 * 
 * @see http://bugs.eclipse.org/337037
 * @see http://wiki.eclipse.org/EclipseLink/Examples/Distributed
 * @author Michael O'Brien
 * @since EclipseLink 2.3
 */
//@Servlet
public class FrontController extends HttpServlet {
    private static final long serialVersionUID = -312633509671504746L;
    
    @EJB(name="ejb/CollatzFacade")
    private CollatzFacadeLocal collatzFacade;

    public static final String EMPTY_STRING = "";
    public static final String FRONT_CONTROLLER_ACTION = "action";
    public static final String FRONT_CONTROLLER_ACTION_DEMO = "demo";
    public static final String FRONT_CONTROLLER_ACTION_GET_STATISTIC = "getStatistic";    

    /**
     * @see HttpServlet#HttpServlet()
     */
    public FrontController() {
        super();
    }
    
    public String getCurrentNumber() {
        return collatzFacade.getCurrentNumberDelimited();
    }

    public String getInterval() {
        return collatzFacade.getPartitionLengthDelimited();
    }

    public String getMips() {
        return collatzFacade.getMipsDelimited();
    }

    // read only
    public String getWorkUnits() {
        return String.valueOf(collatzFacade.getWorkUnits());
    }

    public String getMaxPath() {
        return collatzFacade.getMaxPathDelimited();
    }

    public String getMaxValue() {
        return collatzFacade.getMaxValueDelimited();
    }

    public String getProcessors() {
        return String.valueOf(collatzFacade.getNumberProcessors());
    }

    private void processGetStatistic(HttpServletRequest request, HttpServletResponse response, PrintWriter out) {
        String cell = request.getParameter("cell");
        int cellNumber = -1;
        if(null != cell) {
            cellNumber = Integer.parseInt(cell);
        }
        StringBuffer xmlBuffer = new StringBuffer();
        //long number = System.nanoTime();
        xmlBuffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        xmlBuffer.append("<state>");
        switch (cellNumber) {
            case 0:
                //long random = Math.round(Math.random() * 1000);
                xmlBuffer.append(this.getInterval());
                break;
            case 1:
                xmlBuffer.append(getCurrentNumber());
                break;
            case 2:
                xmlBuffer.append(this.getMaxPath());
                break;
            case 3:
                xmlBuffer.append(this.getMaxValue());
                break;
            case 4:
                xmlBuffer.append(this.getWorkUnits());
                break;
            case 5:
                xmlBuffer.append(this.getMips());
                break;
/*            default:
                xmlBuffer.append(number);
                break;*/
        }
        xmlBuffer.append("</state>");
        out.println(xmlBuffer.toString());        
        //StringBuffer outBuffer = new StringBuffer("Thread: ");        
        //System.out.println("_xml: " + xmlBuffer.toString());
    }
    
    private void processDemoCommand(HttpServletRequest request, HttpServletResponse response, PrintWriter out) {
        String cell = request.getParameter("cell");
        int cellNumber = -1;
        if(null != cell) {
            cellNumber = Integer.parseInt(cell);
        }
        
        StringBuffer xmlBuffer = new StringBuffer();
        long number = System.nanoTime();
        xmlBuffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        xmlBuffer.append("<state>");
        switch (cellNumber) {
        case 0:
            long random = Math.round(Math.random() * 1000);
            xmlBuffer.append(random);
            break;
        case 1:
            xmlBuffer.append(number);
            break;
        default:
            xmlBuffer.append(number);
            break;
        }
        xmlBuffer.append("</state>");
        out.println(xmlBuffer.toString());        
        //StringBuffer outBuffer = new StringBuffer("Thread: ");        
        System.out.println("_xml: " + xmlBuffer.toString());
    }
    
    private void processAction(HttpServletRequest aRequest, HttpServletResponse aResponse) {
        PrintWriter out = null;
        try {
                //HttpSession aSession = aRequest.getSession(true);
                String action = aRequest.getParameter(FRONT_CONTROLLER_ACTION);
                if(null == action) {
                    action = FRONT_CONTROLLER_ACTION_DEMO;
                }
                // Process requests
                if(action.equalsIgnoreCase(FRONT_CONTROLLER_ACTION_DEMO)) {
                    aResponse.setContentType("text/xml");
                    Writer writer = new BufferedWriter(new OutputStreamWriter(aResponse.getOutputStream(),"UTF-8"));
                    out = new PrintWriter(writer, true);
                    processDemoCommand(aRequest, aResponse, out);
                }
                if(action.equalsIgnoreCase(FRONT_CONTROLLER_ACTION_GET_STATISTIC)) {
                    aResponse.setContentType("text/xml");
                    Writer writer = new BufferedWriter(new OutputStreamWriter(aResponse.getOutputStream(),"UTF-8"));
                    out = new PrintWriter(writer, true);
                    processGetStatistic(aRequest, aResponse, out);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
    }
    
	/**
	 * @see Servlet#init(ServletConfig)
	 */
	@Override
    public void init(ServletConfig config) throws ServletException {
	}

	/**
	 * @see Servlet#getServletConfig()
	 */
	@Override
    public ServletConfig getServletConfig() {
		return null;
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        processAction(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        processAction(request, response);
	    
	}

	/**
	 * @see HttpServlet#doPut(HttpServletRequest, HttpServletResponse)
	 */
	@Override
    protected void doPut(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	}

	/**
	 * @see HttpServlet#doDelete(HttpServletRequest, HttpServletResponse)
	 */
	@Override
    protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	}

	/**
	 * @see HttpServlet#doHead(HttpServletRequest, HttpServletResponse)
	 */
	@Override
    protected void doHead(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	}
}
