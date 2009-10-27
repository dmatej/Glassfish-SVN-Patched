/*
 * @(#)file      HtmlMasterPage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.21
 * @(#)lastedit      07/03/08
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 *
 */


package com.sun.jdmk.comm;



// java import
//
import java.util.Set;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MalformedObjectNameException;


class HtmlMasterPage extends HtmlPage {


    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------

    /**
     * Construct a new HtmlMasterPage.
     */
    public HtmlMasterPage(MBeanServer f, boolean r, boolean w) {
        super(f,r,w);
    }


    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------

    public void buildPage(String req) {
        if (logger.finerOn()) {
            logger.finer("buildPage","Handle request = "+req);
        }
        
        // Build the header of the page.
        //
        add2Page(buildHead("["+HtmlDef.jdmkVersion+"] "+HtmlDef.masterPageTitle));
        add2Page(startBody(null));
        
        add2Page("<TABLE WIDTH=100%>");
        add2Page("<TR>");
        add2Page("<TD VALIGN=middle><H2>"+HtmlDef.masterPageTitle+"</H2></TD>");
        add2Page("<TD ALIGN=right VALIGN=top>["+HtmlDef.jdmkVersion+"]</TD>");
        add2Page("</TR></TABLE>");
        
        // Add the Filter.
        //
        add2Page("<FORM ACTION="+HtmlDef.FILTER+" METHOD=GET>");

        ObjectName filter = null;
        if (req.startsWith(HtmlDef.FILTER)){
            String filterStr = decodeUrl(req.substring(HtmlDef.FILTER.length()+1+5));
            try {
                filter = new ObjectName(filterStr);
            } catch (MalformedObjectNameException e) {
                if (logger.finestOn()) {
                    logger.finest("buildPage","Exception = "+e);
                }
                buildError("Invalid Filter ["+filterStr+"]", HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
                return;
            }
            add2Page("Filter by object name: <INPUT type=text name=fstr value="+filterStr+">");
        } else {
            add2Page("Filter by object name: <INPUT type=text name=fstr value=*:*>");
        }
        add2Page("</FORM><BR>");
        Set allObjName = null;
    
        add2Page("<TABLE WIDTH=100%>");
        add2Page("<TR>");
        add2Page("<TD>");
        add2Page("This agent is registered on the domain ");
        add2Page("<STRONG><EM>" + mbs.getDefaultDomain() + "</EM></STRONG>."); 
        allObjName = mbs.queryNames(filter, null);
 
        if (allObjName == null) {
            if (logger.finerOn()) {
                logger.finer("buildPage", "no objects");
            }

            // Build an empty page saying that
            // no object is recorded.
            //
            add2Page("<BR>This page contains no MBeans.</TD></TR>");
            add2Page("</TABLE>");
            add2Page("<HR>");
            htmlPage.append(stopBody());
        } else {
            if (logger.finerOn()) {
                logger.finer("buildPage", allObjName.size()+" object(s)");
            }
    
            add2Page("<BR>This page contains " + 
                     "<STRONG>" + allObjName.size() + "</STRONG>" +
                     " MBean(s).</TD>");
    
            // Add a button to invoke special admin on CMF
            // Only if we are not in read only mode.
            //
            add2Page("<TD ALIGN=\"right\">");
            if (writePerm) {
                add2Page("<FORM ACTION=\""+HtmlDef.ADMIN+HtmlDef.ADMIN_MAIN+"/\" METHOD=GET>");
                add2Page("<INPUT TYPE=submit VALUE=\"Admin\">");
                add2Page("</FORM>");
                add2Page("</TD></TR>");
            }
            add2Page("</TABLE>" );
  
            // Set a horizontal bar.
            //
            add2Page("<HR>");
    
            // Creare a reference list on all objectName.
            // TODO: better presentation which separate domains and services.
            //
            add2Page("<H4>List of registered MBeans by domain:</H4>");
    
            String  currDom  = null; 
            boolean finish   = false;
            int     y = allObjName.size();
            int index,j;
            String  dom, str;
            
            String[] allName = new String[y];
            Object[] allOName = allObjName.toArray();
            for ( j = y-1; j >=0 ; j--) allName[j] = allOName[j].toString(); 
      
            quicksort(allName,0,y-1);
      
            while (!finish) {
                // Look for all elements within the same domain
                // Print and then remove them
                j = 0;
                while (j < y) {
                    String     currElt = allName[j];
    
                    index = currElt.indexOf(':');
                    if (index < 0) {
                        index = 0;
                    }
    
                    dom  = currElt.substring(0, index);
                    str  = currElt.substring(index + 1);
                    str  = translateNameToHtmlFormat(str);
    
                    if (!dom.equals(currDom) || currDom == null) {
                        if (currDom == null){
                            add2Page("<UL type=circle>");
                        } else {
                            add2Page("</UL><P>" );
                        }
                        currDom = dom;  
                        add2Page("<LI><STRONG>" + currDom + "</STRONG>");
                        add2Page("<UL type=disc>");
                    }
                    add2Page("<LI><A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(currElt)+"\">"+str+"</A>");
                    j++;
          
                }
                if (j == y) {
                    finish = true;
                    add2Page("</UL></UL>");
                } else {
                    currDom = null;
                }
            }
            htmlPage.append(stopBody());
        }
    }


}
