/*
 * @(#)file      HtmlAdminPage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.38
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
import java.lang.reflect.Constructor;
import java.util.Set;
import java.util.Date;
import java.util.TimeZone;
import java.text.DateFormat;
import java.util.ArrayList;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.ObjectInstance;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import javax.management.MBeanException;
import javax.management.MBeanRegistrationException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.NotCompliantMBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.RuntimeOperationsException;
import javax.management.MBeanParameterInfo;
import javax.management.JMRuntimeException;
import javax.management.MBeanServerFactory;
import javax.management.loading.ClassLoaderRepository;

// RI import
//
import com.sun.jdmk.Introspector;
import com.sun.jdmk.internal.ClassLogger;

class HtmlAdminPage extends HtmlPage {

     
// --------------------------------------------------------
// CONSTRUCTORS
// --------------------------------------------------------

    /**
     * Constructs a new HtmlAdminPage.
     */
    public HtmlAdminPage(MBeanServer f, boolean r, boolean w) {
        super(f,r,w);
    }


// --------------------------------------------------------
// PUBLIC METHODS
// --------------------------------------------------------
  
    public void buildPage(String req) {
        if (logger.finerOn()) {
            logger.finer("buildPage","Handle the request ["+req+"]");
        }

        if (req.startsWith(HtmlDef.ADMIN_MAIN)) {
            buildCmfPage();
        }
        else if (req.startsWith(HtmlDef.ADMIN_OBJECT)) {
      
            // Parse the req to get the parameters.
            //
            StringBuffer errBuf      = new StringBuffer();
            StringBuffer htmlBuf     = new StringBuffer();
            String       domainName  = null;
            String       className   = null;
            String       cloaderName = null;
            String       moName      = null;
            String       keysName    = null;
            String       action      = null;
            String       params      = "";

            errBuf.append("<HR><P>"+HtmlDef.CRLF + "<FONT SIZE=+3 COLOR=red><B>Administration Request Failed</B></FONT><P><HR><P>");

            // Remove the admin page request part.
            //
            req = req.substring(HtmlDef.ADMIN_OBJECT.length());
            if (logger.finerOn()) {
                logger.finer("buildPage","The admin request ["+req+"]");
            }
            
            // Handle invalid request case.
            //
            if (!(req.startsWith(HtmlDef.ADMIN_QUEST) || req.startsWith(HtmlDef.ADMIN_QUEST2))) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","Invalid command = "+HtmlDef.ADMIN+HtmlDef.ADMIN_OBJECT);
                }
                buildError("Request badly formatted :<BR>"+req,HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                return;
            }

            boolean isreq2 = req.startsWith(HtmlDef.ADMIN_QUEST2);
            if (logger.finerOn()) {
                logger.finer("buildPage","Admin request [type="+((isreq2) ? "2" : "1")+"]");
            }

            // Retrieve the request part.
            //
            req = req.substring(HtmlDef.ADMIN_QUEST.length());

            // Retrieve the parameters part,
            // if it applies.
            //
            if (req.indexOf('?',0)!=-1) {
                params = req.substring(req.indexOf('?',0), req.length());
            }

            if (logger.finerOn()) {
                logger.finer("buildPage","The request parameter list ["+params+"]");
            }

            
            boolean ok = false;
            int     id;
            
            while (!ok) {
                id = req.indexOf('&');
                
                if (id < 0) {
                    ok = true;
                    id = req.length();
                }  
          
                if (req.startsWith("domainName=")) {
                    if (!req.startsWith("&") && (id != 11)) {
                        domainName = req.substring(11, id);
                    }
                }
                else if (req.startsWith("cloaderName=")) {
                    if (!req.startsWith("&") && (id != 12)) {
                        cloaderName = req.substring(12, id);
                    }
                }
                else if (req.startsWith("className=")) {
                    if (!req.startsWith("&") && (id != 10)) {
                        className = req.substring(10, id);
                    }
                }
                else if (req.startsWith("action=")) {
                    if (!req.startsWith("&") && (id != 7)) {
                        action = req.substring(7, id);
                    }
                }
                else if (req.startsWith("keysName=")) {
                    if (!req.startsWith("&") && (id != 9)) {
                        keysName = req.substring(9, id);
                    }
                }
          
                if (!ok) {
                    req = req.substring(id + 1);
                }
            }

            // Check the result.
            //
            ok = true;
        
            if (domainName == null) {
                ok = false;
                errBuf.append("Domain Name cannot be null.<p>");
            } else {
                domainName = decodeUrl(domainName);
                domainName = domainName.trim();
            }

            if (action == null) {
                ok = false;
                errBuf.append("Action cannot be null.<p>");
            } else {
                action = decodeUrl(action);
                action = action.trim();
            }

	    if(action != null && !action.equals(HtmlDef.actionDelete)) {
		if (className == null) {
		    ok = false;
		    errBuf.append("Class cannot be null.<p>");
		} else {
		    className = decodeUrl(className);
		    className = className.trim();
		}
	    }
        
            if (cloaderName != null) {
                cloaderName = decodeUrl(cloaderName);
                cloaderName = cloaderName.trim();
            }
      
            if (keysName == null) {
                ok = false;
                errBuf.append("Keys cannot be null.<p>");
            } else {
                keysName = decodeUrl(keysName);
                keysName = keysName.trim();
            }

            // Look for constructor paramaters in the request.
            //
            ArrayList valueList = new ArrayList();
            ArrayList typeList  = new ArrayList();

            if (ok && action.equals(HtmlDef.actionAdd) && isreq2 && params.indexOf('?',0)!=-1 && params.indexOf('=',1)!=-1) {

                String  reqStr = params.substring(params.indexOf('?',0)+1);
                String  typeStr;
                String  valueStr;
                String  propStr;
    
                int     index;
                boolean done       = false;
                String  propReqStr = null;

                while (!done) {
                    // A property name separator is always "&".
                    //
                    index = reqStr.indexOf('&');
                    if (index < 0) {
                        // No more entry, after this one.
                        //
                        propReqStr = reqStr;
                        done       = true;
                    } else {
                        propReqStr = reqStr.substring(0, index);
                        reqStr     = reqStr.substring(index + 1);
                    }
 
                    // Parse this one.
                    //
                    index = propReqStr.indexOf('=');
      
                    if (index < 0) {
                        done = true;
                        // We cannot continue.
                        //
                        ok = false;
                        errBuf.append("Syntax error in request ["+propReqStr+"] "+HtmlDef.HTTP_ERROR_BAD_REQUEST_ID+" "+HtmlDef.HTTP_ERROR_BAD_REQUEST);
                        break;
                    }
      
                    propStr  = propReqStr.substring(0, index);
                    valueStr = propReqStr.substring(index + 1);
                    typeStr  = null;
      
                    if ((propStr != null) && (propStr.length() != 0)) {
                        // Remove %0D in the begining of property name.
                        //
                        while (propStr.startsWith("%")) {
                            propStr = propStr.substring(3);
                        }
                        // Substitute the %HH character on valueStr.
                        //
                        if (valueStr != null && valueStr.length() != 0 )
                            valueStr = decodeUrl(valueStr);
                        else
                            valueStr = HtmlDef.NULL;
    
                        // The propertyName is build with <propertyName>%2B<value type>.
                        //
    
                        if (logger.finerOn()) {
                            logger.finer("buildPage","Parsing property name ["+propStr+"]");
                        }
    
                        index = propStr.indexOf("%2B", 0);
                        if (index < 0) {
                            index = propStr.indexOf("%2b", 0);
                        }
    
                        if (index >= 0) {
                            typeStr = propStr.substring(index + 3);
                            propStr = propStr.substring(0, index);
                        } else {
                            typeStr = "String";
                        }
    
                        if (logger.finerOn()) {
                            logger.finer("buildPage","Found the property "+propStr+" ["+"Type="+typeStr+", "+" Value="+valueStr+"]");
                        }
                
                        // Save the pair elements
                        // and filter action field.
                        //
                        if (!propStr.equals("action")) {
                            typeList.add(typeStr);
                            valueList.add(valueStr);
                        }
                    } else {
                        if (logger.finerOn()) {
                            logger.finer("buildPage","Got null value or property");
                        }
                        ok = false;
                        errBuf.append("Syntax error in request ["+propReqStr+"]"+HtmlDef.HTTP_ERROR_BAD_REQUEST_ID+" "+HtmlDef.HTTP_ERROR_BAD_REQUEST);
                        break;
                    }
                }
                if (logger.finerOn()) {
                    logger.finer("buildPage","Found "+valueList.size()+" parameters");
                }
            }

            if (ok) {
                errBuf = null;
                // Execute the request.
                //
                errBuf = cmfHttpAction(action, domainName, className, keysName, cloaderName, typeList, valueList, isreq2);
            }
      
            String nm = domainName+":";
            if (keysName != null) {
                nm += keysName;
            }
	    String safeObjNameStr = translateNameToHtmlFormat(nm);

            if (action.equals(HtmlDef.actionDelete)) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","Build the unregister result page");
                }

                // Build the header of the page.
                //
                if (!isreq2) {
                    buildCmfPage(domainName, className, keysName, cloaderName, action, false);
                    add2Page("<TABLE WIDTH=100%><TR>");
                    add2Page("<TD ALIGN=LEFT><H3><STRONG>"+HtmlDef.actionDelete+" Result:</STRONG></H3></TD>");
                    add2Page("</TR></TABLE>");
                } else {
                    add2Page(buildHead("["+HtmlDef.jdmkVersion+"] "+HtmlDef.adminPageTitle));
                    add2Page(startBody(null)); // color = null
                }

                // Build the result of the page.
                //
                if (errBuf == null) {
                    htmlPage.append("<HR><P>"+HtmlDef.CRLF +
                        "<FONT SIZE=+3 COLOR=green><B>Unregister Successful" +
                        "</B></FONT><P><HR><P>" + "The MBean ["+safeObjNameStr+"] was successfully unregistered.");
                } else {
                    add2Page(errBuf.toString());
                }
                if (isreq2) {
                    add2Page("<P><TABLE WIDTH=100%><TR>");      
                    add2Page("<TD ALIGN=RIGHT>"+HtmlDef.LISTOFMBEAN+"</TD>");
                    add2Page("</TR></TABLE>");
                }
            } else if (action.equals(HtmlDef.actionAdd)) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","Build the create result page");
                }

                buildCmfPage(domainName, className, keysName, cloaderName, action, false);
                if (isreq2) {
                    buildCtorsList(domainName, className, keysName, cloaderName);
                }

                add2Page("<TABLE WIDTH=100%><TR>");
                add2Page("<TD ALIGN=LEFT><H3><STRONG>"+HtmlDef.actionAdd+" Result:</STRONG></H3></TD>");
                add2Page("</TR></TABLE>");

                if (errBuf == null) {
                    htmlPage.append("<HR><P>"+HtmlDef.CRLF +
                        "<FONT SIZE=+3 COLOR=green><B>Create Successful" +
                        "</B></FONT><P><HR><P>" + "The MBean ["+safeObjNameStr+"] was successfully instantiated and registered.");
                    add2Page("<P><TABLE WIDTH=100%><TR>");                    
                    add2Page("<TD ALIGN=RIGHT><A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(nm)+"\">Go to "+HtmlDef.objectPageTitle+"</A></TD>");
                    add2Page("</TR></TABLE>");
                } else {
                    add2Page(errBuf.toString());
                }
            } else if (action.equals(HtmlDef.actionConstructors)) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","Build the constructors result page");
                }

                buildCmfPage(domainName, className, keysName, cloaderName, action, false);

                if (errBuf == null) {
                    buildCtorsList(domainName, className, keysName, cloaderName);
                }

                add2Page("<TABLE WIDTH=100%><TR>");
                add2Page("<TD ALIGN=LEFT><H3><STRONG>"+HtmlDef.actionConstructors+" Result:</STRONG></H3></TD>");
                add2Page("</TR></TABLE>");

                if (errBuf == null) {
                    htmlPage.append("<HR><P>"+HtmlDef.CRLF +
                        "<FONT SIZE=+3 COLOR=green><B>List Constructors Successful" +
                        "</B></FONT><P><HR><P>" + "The list of public constructors was successfully build for the MBean ["+safeObjNameStr+"].");
                } else {
                    add2Page(errBuf.toString());
                }
            }
            add2Page(stopBody());
        } else {
            // Bad Request.
            //
            buildError(req,HtmlDef.HTTP_ERROR_BAD_REQUEST_ID+" "+HtmlDef.HTTP_ERROR_BAD_REQUEST);
        }
    }


// --------------------------------------------------------
// PRIVATE METHODS
// --------------------------------------------------------

    private void buildCmfPage() {
        buildCmfPage(null, null, null, null, null, true);
    }
  
    private void buildCmfPage(String domainName, String className,
                              String keysName, String cloaderName, String action, boolean withEndPage) {

        // Build the header of the page
        //
        add2Page(buildHead("["+HtmlDef.jdmkVersion+"] "+HtmlDef.adminPageTitle));
        add2Page(startBody(null)); // color = null
 
        // Build the title of the page.
        //
        add2Page("<TABLE WIDTH=100%>");
        add2Page("<TR>");
        add2Page("<TD ALIGN=left><H2>"+HtmlDef.adminPageTitle+"</H2></TD>");
        add2Page("<TD ALIGN=right VALIGN=top>["+HtmlDef.jdmkVersion+"]</TD>");
        add2Page("</TR></TABLE>");    
    
        // Part for registering objects
        //
        add2Page("<TABLE WIDTH=100%><TR>");
        add2Page("<TD ALIGN=RIGHT>"+HtmlDef.LISTOFMBEAN+"</TD>");
        add2Page("</TR></TABLE>");
    
        // Build a FORM with:
        //
        // Domain       :
        // Keys         :
        //
        // Java Class   :
        //
        // Class Loader :
        //
        add2Page("<P>Specify the object name and java class of the MBean to add, delete or view the constructors of:<BR>");
        add2Page("<I>(Optionally provide a class loader name for loading the specified class.)</I><BR>");
        add2Page("<FORM ACTION=/Admin/Objects/ METHOD=GET>");

        // --------------------------------------------
        // part 1
        //
        add2Page("<TABLE ALIGN=center BORDER=1 WIDTH=480>" );
        add2Page("<TR>") ;
        add2Page("<TD>") ;
        add2Page("<TABLE>");
    
        // -------------
        // Row 1 
        //
        if (domainName == null) {
            domainName = mbs.getDefaultDomain();
        } 
        addRow ( "Domain" , "domainName" , translateNameToHtmlFormat(domainName)) ;
   
        // -------------
        // Row 2 
        //
        addRow ( "Keys" , "keysName" , translateNameToHtmlFormat(keysName)) ;
        add2Page("</TABLE>");
        add2Page("</TD>") ;
        add2Page("</TR>") ;

        // --------------------------------------------
        // part 2
        //
        add2Page("<TR>") ;
        add2Page("<TD>") ;
        add2Page("<TABLE>");
    
        // -------------
        // Row 1 
        //
        addRow ( "Java Class" , "className" , className) ;
        add2Page("</TABLE>");
        add2Page("</TD>") ;
        add2Page("</TR>") ;

        // --------------------------------------------
        // part 3
        //
        add2Page("<TR>") ;
        add2Page("<TD>") ;
        add2Page("<TABLE>");
    
        // -------------
        // Row 1 
        //
        addRow ( "Class Loader" , "cloaderName" , cloaderName) ;
        add2Page("</TABLE>");
        add2Page("</TD>") ;
        add2Page("</TR>") ;
        add2Page("</TABLE>");
    
        // --------------------------------------------
        // Table 4
        //Action Button
        //
        add2Page("<TABLE WIDTH=480 CELLPADDING=0 CELLSPACING=6>");
        add2Page("<TR>");
        add2Page("<TH ALIGN=RIGHT>Action:</TH>");
        add2Page("<TD>");
        add2Page("<SELECT NAME=\"action\">");
    
        if (action == null || action.equals(HtmlDef.actionAdd)) {
            // Set Add by default
            //
            add2Page("<OPTION SELECTED>" + HtmlDef.actionAdd);
            add2Page("<OPTION>" + HtmlDef.actionDelete);
            add2Page("<OPTION>" + HtmlDef.actionConstructors);
        } 
        else if (action.equals(HtmlDef.actionDelete)) {
            add2Page("<OPTION>" + HtmlDef.actionAdd);
            add2Page("<OPTION SELECTED>" + HtmlDef.actionDelete);
            add2Page("<OPTION>" + HtmlDef.actionConstructors);
        } 
        else if (action.equals(HtmlDef.actionConstructors)) {
            add2Page("<OPTION>" + HtmlDef.actionAdd);
            add2Page("<OPTION>" + HtmlDef.actionDelete);
            add2Page("<OPTION SELECTED>" + HtmlDef.actionConstructors);
        }
        else {
            // Set Add by default
            //
            add2Page("<OPTION SELECTED>" + HtmlDef.actionAdd);
            add2Page("<OPTION>" + HtmlDef.actionDelete);
            add2Page("<OPTION>" + HtmlDef.actionConstructors);
        }
 
        add2Page("</SELECT>");
        add2Page("</TD></TR>");
        //
        add2Page("<TR>");
        add2Page("<TD> </TD>");
        add2Page("<TD>");
        add2Page("<INPUT TYPE=SUBMIT VALUE=\"" + HtmlDef.sendReqBut + "\">");
        add2Page("<INPUT TYPE=RESET VALUE=\"" + HtmlDef.resetBut + "\">");
        add2Page("</TD></TR>");
        //
        add2Page("</TABLE>");
        add2Page("</FORM>");
        add2Page("<HR>");
    
        if (withEndPage) {
            // All ends remains the same
            //
            add2Page(HtmlDef.endPage);
        }
    }
  
    private StringBuffer cmfHttpAction(String action, String domainName, String className,
                                       String keysName, String cloaderName, ArrayList typeList, ArrayList valueList, boolean bctor) {
    
	// Convert object name string to be HTML safe
	String safeDomainName = translateNameToHtmlFormat(domainName) ;
	String safeKeysName   = translateNameToHtmlFormat(keysName) ;
  
        StringBuffer errBuf = new StringBuffer();
        errBuf.append("<HR><P><FONT SIZE=+3 COLOR=red><B>");
        boolean error  = false;
    
        if (action == null) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", HtmlDef.HTTP_ERROR_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_NOT_FOUND + "Undefined action");
            }
            errBuf.append(HtmlDef.HTTP_ERROR_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_NOT_FOUND + "</B></FONT><P><HR><P>" + "<P>Undefined action");
            return errBuf;
        }

        if (logger.finerOn()) {
            logger.finer("cmfHttpAction","Perform the operation ["+action+"]");
        }
 
        ObjectName objName = null;

        // We have to build the object name.
        //
        try {
            objName = new ObjectName(domainName + ":" + keysName);
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME+" Cannot create the ObjectName ["+domainName+":"+keysName+"]");
            }
            errBuf.append(HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME + "</B></FONT><P><HR><P>" +"<P>Cannot create the ObjectName ["+ safeDomainName +":"+ safeKeysName +"]");
            return errBuf;
        }

        // Build the classloader object name.
        //
        ObjectName ocloaderName = null;
        if (cloaderName != null && !cloaderName.equals("")) {
            try {
                ocloaderName = new ObjectName(cloaderName);
            } catch (MalformedObjectNameException e) {
                if (logger.finestOn()) {
                    logger.finest("cmfHttpAction", HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME+" Cannot create the ObjectName for the classloader specified ["+cloaderName+"]");
                }
                errBuf.append(HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME + "</B></FONT><P><HR><P>" +"<P>Cannot create the ObjectName for the classloader specified ["+cloaderName+"]");
                return errBuf;
            }
        }

        try {
            if (action.equals(HtmlDef.actionDelete)) {
                mbs.unregisterMBean(objName);
                if (logger.finerOn()) {
                    logger.finer("cmfHttpAction","Unregistered the MBean ["+objName+"]");
                }
            }
            else if (action.equals(HtmlDef.actionAdd)) {
                if (!valueList.isEmpty()) {
                    int nbElements = valueList.size();
                    Object[] argu = new Object[nbElements];
                    String[] sign = new String[nbElements];
                    for (int i = 0; i < nbElements ; i++) {
                        String val = (String)valueList.get(i);
                        String type = (String) typeList.get(i);
                        sign[i]=type;

                        if (logger.finerOn()) {
                            logger.finer("cmfHttpAction","The MBean ["+objName+"] constructor paramater "+i+" [Type="+type+", Value="+val+"]");
                        }

                        if (val.equals(HtmlDef.NULL)){
                            argu[i] = (Object) null;
                            continue;
                        }

                        if (type.endsWith("String")) {
                            argu[i] = val;
                        }
                        else if (type.endsWith("Boolean") || type.endsWith("boolean")) {
                            argu[i] = new Boolean(val);
                        }
                        else if (type.endsWith("Byte") || type.endsWith("byte")) {
                            argu[i] = new Byte(val);
                        }
                        else if (type.equals("javax.management.ObjectName")) {
                            argu[i] = new ObjectName(val);
                        }
                        else if (type.endsWith("Integer") || type.endsWith("int")) {          
                            argu[i] = new Integer(val);
                        }
                        else if (type.endsWith("Character") || type.endsWith("char") ){
                            argu[i] = new Character(val.charAt(0));
                        }
                        else if (type.endsWith("Long") || type.endsWith("long")) {
                            argu[i] = new Long(val);
                        }
                        else if (type.endsWith("Double") || type.endsWith("double")) {
                            argu[i] = new Double(val);
                        }
                        else if (type.endsWith("Float") || type.endsWith("float")) {
                            argu[i] = new Float(val);
                        }
                        else if (type.endsWith("Short") || type.endsWith("short")) {
                            argu[i] = new Short(val);
                        }
                        else if (type.endsWith("Date")) {
                            try {
                                DateFormat df = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);
                                // BUG : the TZ of the DateFormat is not set correctly with the method getDateTimeInstance.
                                // By default, the DateFormat use the TZ of the system.
                                df.setTimeZone(TimeZone.getDefault());
                                argu[i] = df.parse(val);
                            } catch (java.text.ParseException e){
                                errBuf.append(HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID + " " + HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE + "</B></FONT><P><HR><P>" + "Cannot convert String to "+type);
                                error = true;
                                break;
                            }
                        }
                        else if (type.endsWith("Number")) {
                            try {
                                argu[i] = new Integer(val) ;
                            } catch(NumberFormatException e1) {
                                try {
                                    argu[i] = new Long(val) ;
                                } catch(NumberFormatException e2) {
                                    try {
                                        argu[i] = new Float(val) ;
                                    } catch(NumberFormatException e3) {
                                        try {
                                            argu[i] = new Double(val) ;
                                        } catch(NumberFormatException e4){
                                            errBuf.append(HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID + " " + HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE + "</B></FONT><P><HR><P>" + "Cannot convert String to "+type);
                                            error = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                        else {
                            errBuf.append(HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID + " " + HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE + "</B></FONT><P><HR><P>" + "Cannot convert String to "+type);
                            error = true;
                            break;
                        }
                    }

                    if (!error) {
                        if (ocloaderName != null ) {
                            if (logger.finerOn()) {
                                logger.finer("cmfHttpAction","Create the MBean ["+objName+"] with the specified constructor and using the class loader ["+ocloaderName+"]");
                            }
                            mbs.createMBean(className, objName, ocloaderName, argu, sign);
                        } else {
                            if (logger.finerOn()) {
                                logger.finer("cmfHttpAction","Create the MBean ["+objName+"] with the specified constructor and using the default loader repository");
                            }
                            mbs.createMBean(className, objName, argu, sign);
                        }
                    }
                } else {
                    if (ocloaderName != null ) {
                        if (logger.finerOn()) {
                            logger.finer("cmfHttpAction","Create the MBean ["+objName+"] with the default constructor and using the class loader ["+ocloaderName+"]");
                        }
                        mbs.createMBean(className, objName, ocloaderName);
                    } else {
                        if (logger.finerOn()) {
                            logger.finer("cmfHttpAction","Create the MBean ["+objName+"] with the default constructor and using the default loader repository");
                        }
                        mbs.createMBean(className, objName);
                    }
                }
            }
            if (action.equals(HtmlDef.actionConstructors) || (bctor && action.equals(HtmlDef.actionAdd))) {
                if (logger.finerOn()) {
                    logger.finer("cmfHttpAction","List the constructors of the MBean ["+objName+"] using the class loader ["+ocloaderName+"]");
                } 
                ctors = getConstructors(findClass(className, ocloaderName));
            }
        } catch (ReflectionException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "Reflection exceptoin. [Exception="+e.getTargetException()+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_REFLECTION_ID + " " + HtmlDef.HTTP_ERROR_REFLECTION + "</B></FONT><P><HR><P>" + e.getTargetException());
        } catch (InstanceAlreadyExistsException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "Instance already exists. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_INSTANCE_ALREADY_EXISTS_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_ALREADY_EXISTS + "</B></FONT><P><HR><P>" + e.toString());
        } catch (MBeanRegistrationException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "MBean registration. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_MBEAN_REGISTRATION_ID + " " + HtmlDef.HTTP_ERROR_MBEAN_REGISTRATION + "</B></FONT><P><HR><P>" + e.toString());
        } catch (MBeanException  e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "MBean exception. [Exception="+e.getTargetException()+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_MBEAN_ID + " " + HtmlDef.HTTP_ERROR_MBEAN + "</B></FONT><P><HR><P>" + e.getTargetException());
        } catch (NotCompliantMBeanException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "Not Compliant MBean. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_NOT_COMPLIANT_MBEAN_ID + " " + HtmlDef.HTTP_ERROR_NOT_COMPLIANT_MBEAN + "</B></FONT><P><HR><P>" + e);
        } catch (InstanceNotFoundException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "Instance not found. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND + "</B></FONT><P><HR><P>" + e);
         } catch (ClassNotFoundException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "The MBean class could not be loaded. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_CLASS_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_CLASS_NOT_FOUND + "</B></FONT><P><HR><P>" + e);
        } catch (NumberFormatException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Cannot convert String to type. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST + "</B></FONT><P><HR><P>" + e);
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Malformed object name. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME + "</B></FONT><P><HR><P>" + e);
        } catch (JMRuntimeException e) {
            if (logger.finestOn()) {
                logger.finest("cmfHttpAction", "Instance not found. [Exception="+e+"]");
            }
            error = true;
            errBuf.append(HtmlDef.HTTP_ERROR_MBEAN_ID + " " + HtmlDef.HTTP_ERROR_MBEAN + "</B></FONT><P><HR><P>" + e);
        }

    
        if (error)
            return errBuf;
        else
            return (StringBuffer) null;
    }
  
    private void addRow(String fieldName, String fieldarg, String name) {
        String actualName = null ;

        if (name == null) {
            actualName = "";
        } else {
            actualName =  name ;
        }
        add2Page("<TR>") ;
        
        // -------------
        // column 1
        //
        add2Page("<TD>") ;
        add2Page("<TABLE WIDTH=130 CELLPADDING=0 CELLSPACING=0>") ;
        add2Page("<TR>") ;
        add2Page("<TH ALIGN=RIGHT>" + fieldName + ":</TH>") ;
        add2Page("</TR>") ;
        add2Page("</TABLE>") ;
        add2Page("</TD>") ;

        // -------------
        // column 2
        //
        add2Page("<TD>") ;
        add2Page("<TABLE CELLPADDING=0 CELLSPACING=0>") ;
        add2Page("<TR>") ;
        add2Page("<TD><INPUT TYPE=\"TEXT\" NAME=\"" + fieldarg + "\" VALUE=\"" + actualName + "\"SIZE=\"45\"></TD>") ;
        add2Page("</TR>") ;
        add2Page("</TABLE>") ;
        add2Page("</TD>") ;
        
        add2Page("</TR>") ;
    }

    private void buildCtorsList(String domainName, String className, String keysName, String cloaderName) {

	// Convert object name string to be HTML safe
	String safeDomainName = translateNameToHtmlFormat(domainName) ;
	String safeKeysName   = translateNameToHtmlFormat(keysName) ;
	
        if (ctors != null) {
            if (logger.finerOn()) {
                logger.finer("buildCtorsList","Found "+ctors.length+" constructors(s)");
            }
            String ctorStr = null;

            add2Page("<P><H3>List of MBean constructors for:</H3>"+ HtmlDef.PF);
            add2Page("<UL type=disc><LI><B>MBean Name:</B> " + safeDomainName +":"+ safeKeysName);
            add2Page("<LI><B>Class Loader Name:</B> " + ((cloaderName==null) ? "" : cloaderName));
            add2Page("</UL>");

            for (int i=0; i<ctors.length; i++) {
                ctorStr = buildCtorWithParam(domainName, className, keysName, cloaderName, ctors[i].getSignature());

                if (ctorStr != null) {
                    // Get the constructor description.
                    //
                    String ai = ctors[i].getDescription();
                    if (ai != null && ai.length() > 0) {
                        add2Page("<HR><A HREF=\"javascript:alert('"+ai+"');\"><B>Description of " +ctors[i].getName() + "</B></A>");
                    } else {
                        add2Page("<HR><B>"+ctors[i].getName()+"</B>");
                    }
                    add2Page(ctorStr);
                }
            }
        }
    }

    private String buildCtorWithParam(String domainName, String className, String keysName, String cloaderName, MBeanParameterInfo[] paramList) {
        if (logger.finerOn()) {
            logger.finer("buildCtorWithParam","Build the constructor ["+className+"] with parameters");
        }

	// Convert object name string to be URL encoded
	String encDomainName = encodeUrl(domainName) ;
	String encKeysName   = encodeUrl(keysName) ;

        StringBuffer str = new StringBuffer(50);
        String propType = null;
        String param = null;
        int max= paramList.length;
        boolean support = true;

        // Do we support this constructor ?
        //
        for (int i = 0; i < max; i++) {
            propType = paramList[i].getType();
            if (!checkType(propType) || isArrayType(propType)) {
                support = false;
                break;
            }
        }
        if (!support) {
            str.append("  <I>(Constructor Not Supported)</I>"+ HtmlDef.PF);
        }

        add2Page("<TD>");
        if (support) {
            str.append("<FORM ACTION=\"" + 
                HtmlDef.ADMIN + 
                HtmlDef.ADMIN_OBJECT + 
                HtmlDef.ADMIN_QUEST2 +
                "&domainName=" + encDomainName +
                "&keysName=" + encKeysName +
                "&className=" + className +
                "&cloaderName=" + ((cloaderName!=null) ? cloaderName : "") +
                "&action=" + HtmlDef.actionAdd +
                "&\" METHOD=GET>" + HtmlDef.PF);
        }

        str.append("<TABLE>" + HtmlDef.PF);

        if (!support) {
            str.append("<P><TD><B>"+className+"<B></TD>"+HtmlDef.PF);
        } else {
            str.append("<TD><INPUT TYPE=SUBMIT VALUE=\""+HtmlDef.actionAdd+"\"></TD>"+HtmlDef.PF);
            str.append("<TD><B>"+className+"</B> </TD>");
        }

        // Default constructor.
        //
        if (paramList.length == 0) {
            str.append("<TD>(default)</TD>");
        }

        for (int i = 0; i < max; i++) {
            propType = paramList[i].getType();

            if (paramList[i].getName().length() > 0) {
                param = paramList[i].getName();
            } else {
                param = "param"+i;
            }

            if (logger.finerOn()) {
                logger.finer("buildCtorWithParam","Build the constructor ["+className+"] parameter "+param+" [Type="+propType+"]");
            }

            if (i != 0) str.append("<TD></TD>");
            String ai = paramList[i].getDescription();
            if (ai != null && ai.length() > 0) {
                str.append("<TD>("+propType+")<A HREF=\"javascript:alert('"+ai+"');\">" +param+"</A></TD>"+HtmlDef.PF);
            } else {
                str.append("<TD>("+propType+")" +param+"</TD>"+HtmlDef.PF);
            }    

            if (!support) {
                str.append("<TD></TD>" + HtmlDef.PF);
            } else if (propType.endsWith("Boolean") || propType.endsWith("boolean")) {
                str.append("<TD>" + boolToHtml(param, propType,"true",true) + "</TD>" + HtmlDef.PF);
            } else {
                str.append("<TD><INPUT TYPE=\"text\" NAME=\"" + param +  "+" + propType + "\" ");
                str.append("SIZE=50%");
                str.append( "></TD>" + HtmlDef.PF);    
            }
            str.append("</TR><TR><TD></TD>" + HtmlDef.PF);
        }

        str.append("</TR></TABLE>" + HtmlDef.PF);
        if (support) {
            str.append("</FORM>" + HtmlDef.PF);
        }
        str.append("<HR>");

        return str.toString();
    }

    private MBeanConstructorInfo[] getConstructors(Class baseClass)
        throws NotCompliantMBeanException {

        // Get the MBean information,
        // and retrieve the constructors.
        //
        MBeanInfo info = Introspector.testCompliance(baseClass);
        if (info != null) {
            return info.getConstructors();
        }

        // No MBean information,
        // apply introspection.
        //
        if (logger.finestOn()) {
            logger.finest("getConstructors", "No MBean information, apply introspection on [Class="+baseClass+"]");
        }
	    return getTargetConstructors(baseClass);     
    }

    private MBeanConstructorInfo[] getTargetConstructors(Class baseClass) {
        Constructor[] consList = baseClass.getConstructors();
        MBeanConstructorInfo[] resultConstructors;
        java.util.Vector constructors = new java.util.Vector();
     
        // Now analyze each Constructor.
        //        
        for (int i = 0; i < consList.length; i++) {
	        Constructor constructor = consList[i];    	    
	        MBeanConstructorInfo mc = null;
	        try {               
	            mc = new MBeanConstructorInfo("Public constructor of the MBean", constructor);		     		                
	        } catch (Exception ex) {
	            mc = null;
                if (logger.finestOn()) {
                    logger.finest("getTargetConstructors", "Couldn't construct MBean constructor infor for [Constructor="+constructor+"]");
                }
	        }
	        if (mc != null) {
	            constructors.addElement(mc);
	        }
        }

        // Allocate and populate the result array.
        //
        resultConstructors = new MBeanConstructorInfo[constructors.size()];        
	    for (int i = 0; i < resultConstructors.length; i++) {
	        resultConstructors[i] = (MBeanConstructorInfo)constructors.elementAt(i);
	    }
	    return resultConstructors;     
    }

    private Class findClass(String className) 
        throws ClassNotFoundException {
        if (logger.finerOn()) {
            logger.finer("findClass","Load the class ["+className+"] using the class loader [Default Loader Repository]");
        }
	return loadClass(className);
    }

    private Class findClass(String className, ObjectName aLoader) 
        throws ClassNotFoundException, InstanceNotFoundException,
        MBeanException, ReflectionException  {
    
        if (aLoader == null) {
            return findClass(className);
        }

        Set qresult = null;
        Object[] qmbeans = null;
        synchronized(this) {
            qresult = mbs.queryMBeans(aLoader, null);
        }
        if (qresult.isEmpty()) {
            throw new InstanceNotFoundException("The loader named " + aLoader + " is not registered in the MBeanServer");
        }
        qmbeans = qresult.toArray();
 
        // Ok now we have the service. Let it do the work !
        //
        if (logger.finerOn()) {
            logger.finer("findClass","Load the class ["+className+"] using the class loader ["+((ObjectInstance)qmbeans[0]).getObjectName()+"]");
        }
        Object[] params = new Object[] {className};
        String[] signature = new String[] {"java.lang.String"};
        Class theClass = (Class)mbs.invoke(((ObjectInstance)qmbeans[0]).getObjectName(), "loadClass", params, signature);
        return theClass;
    }

    private Class loadClass(String className) 
	throws ClassNotFoundException {
	try {
	    return Class.forName(className);
	} catch (ClassNotFoundException e) {
	    final ClassLoaderRepository clr = 
		MBeanServerFactory.getClassLoaderRepository(mbs);
	    if (clr == null) throw new ClassNotFoundException(className);
	    return clr.loadClass(className);
	}
    }

// --------------------------------------------------------
// PRIVATE VARIABLES
// --------------------------------------------------------

    final int inputfieldSize = 45;

    private MBeanConstructorInfo[] ctors = null;
}
