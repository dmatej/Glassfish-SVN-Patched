/*
 * @(#)file      HtmlInvokePage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.23
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
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import java.text.DateFormat;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MBeanOperationInfo;
import javax.management.MalformedObjectNameException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.ReflectionException;
import javax.management.JMRuntimeException;

class HtmlInvokePage extends HtmlPage {


    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------

    /**
     * Constructs a new HtmlInvokePage.
     */
    public HtmlInvokePage(MBeanServer f, boolean r, boolean w) {
        super(f,r,w);
    }


    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------
  
    public void buildPage(String req) {
        if (logger.finerOn()) {
            logger.finer("buildPage","Handle request = "+req);
        }
    
        int    index;
        int    query;
        String func;
        String objNameStr;

        index = req.indexOf('/',1);
        query  = req.indexOf("/action=",1);

        // Handle invalid request case.
        //
        if (index < 0 || query < 0) {
            // Bad request.
            //
            buildError("Invalid Request: "+req,HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
            return;
        }

        // Read the name of the operation.
        //
        func = getAction(req.substring(query+1));
        if (func == null) {
            // Bad request.
            //
            buildError("Invalid Request: "+req,HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
            return;
        }

        // Read the list of params.
        //
        if (!getParams(req.substring(req.indexOf('?',1)+1))) {
            return;
        }
    
        // Read the objectName.
        //
        objNameStr = fromUrlName(req.substring(0, query));
	String safeObjNameStr = translateNameToHtmlFormat(objNameStr);

        if (logger.finerOn()) { 
            logger.finer("buildPage","For " +
		  "objName = [" + objNameStr + "] " +
		  "function called = [" + func + "]");
        }
   
        ObjectName objName = null;
        try {
            objName = getObjectNameByObjNameStr(objNameStr);
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("Invalid ObjectName: "+safeObjNameStr,
                       HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
            return;
        }
    
        if (objName == null) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Unable to get Object Definition for ["+objNameStr+"]");
            }
            buildError("Unable to get Object Definition for ["+safeObjNameStr+"]",
                       HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return;
        }
        
        htmlPage.append(buildHead(HtmlDef.topPageTitle + func + " invocation"));
        htmlPage.append(startBody(null)); // null for color

        // Invoke the method.
        //
        int nbElements = valueList.size();
        Object[] argu = new Object[nbElements];
        String[] sign = new String[nbElements];
        for (int i = 0; i < nbElements ; i++) {
            // Convert to the new type if needed.
            //
            String val = (String)valueList.get(i);
            String type = (String) typeList.get(i);
            sign[i]=type;
            
            if (val.equals(HtmlDef.NULL)){
                argu[i] = (Object) null;
                continue;
            }
      
            try {
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
                        buildError("Cannot convert String \""+ val +"\" to " +
                               type + ".<P>",HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                        return;
                    }
                }
                else if (type.endsWith("Number")) {
                    try {
                        argu[i] = new Integer(val) ;
                    }
                    catch(NumberFormatException e1) {
                        try {
                            argu[i] = new Long(val) ;
                        }
                        catch(NumberFormatException e2) {
                            try {
                                argu[i] = new Float(val) ;
                            }
                            catch(NumberFormatException e3) {
                                try {
                                    argu[i] = new Double(val) ;
                                }
                                catch(NumberFormatException e4){
                                    buildError("Cannot convert String \""+ val +"\" to " + type +".<P>",
                                           HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                                    return;
                                }
                            }
                        }
                    }
                }
                else {
                    // Unknown conversion mechanism for this
                    //
                    buildError("Cannot convert String to "+type,
                               HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID + " " + HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE);
                    return;
                }
            } catch (NumberFormatException e) {
                if (logger.finestOn()) {
                    logger.finest("buildPage","Cannot convert String to "+type+" Exception = "+e);
                }
                buildError("Cannot convert String to "+type, HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                return;
            } catch (MalformedObjectNameException e) {
                if (logger.finestOn()) {
                    logger.finest("buildPage","The format of the string ["+objNameStr+"] does not correspond to a valid ObjectName."+" Exception = "+e);
                }
                buildError("The format of the string ["+safeObjNameStr+
                           "] does not correspond to a valid ObjectName <P>" +
                           e.toString() + "<P>",HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
                return;
            }
        }
        Object res = null;
        try {
            res = mbs.invoke(objName,func,argu,sign);
        } catch(InstanceNotFoundException e){
            if (logger.finestOn()) {
                logger.finest("buildPage","Unable to get MBeanInfo for ["+objNameStr+"]"+" Exception = "+e);
            }
            buildError("Unable to get MBeanInfo for ["+safeObjNameStr+"]",HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID+" "+HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return;
        } catch(MBeanException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","The MBean ["+objNameStr+"] throws an MBeanException when calling ["+func+"]"+" Exception = "+e.getTargetException());
            }
            buildError("The MBean ["+safeObjNameStr+"] throws an MBeanException when calling ["+func+"]:<BR>"+e.getTargetException().toString(),HtmlDef.HTTP_ERROR_MBEAN_ID+" "+HtmlDef.HTTP_ERROR_MBEAN);
            return;
        } catch (ReflectionException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","The MBeanServer throws a ReflectionException when calling operation["+func+"] of ["+objNameStr+"]"+" Exception = "+e.getTargetException());
            }
            buildError("The MBeanServer throws an ReflectionException when calling operation["+func+"] of ["+safeObjNameStr+"] :<BR>"+e.getTargetException().toString(),HtmlDef.HTTP_ERROR_REFLECTION_ID+" "+HtmlDef.HTTP_ERROR_REFLECTION);
            return;
        } catch (JMRuntimeException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","The MBeanServer throws a JMRuntimeException when calling operation["+func+"] of ["+objNameStr+"]"+" Exception = "+e);
            }
            buildError("The MBeanServer throws a JMRuntimeException when calling operation["+func+"] of ["+safeObjNameStr+"] :<BR>"+e,HtmlDef.HTTP_ERROR_MBEAN_ID+" "+HtmlDef.HTTP_ERROR_MBEAN);
            return;
        }
        htmlPage.append("<HR><P>"+HtmlDef.CRLF+"<FONT SIZE=+3 COLOR=green><B>"+func+" Successful"+"</B></FONT><P><HR><P>"+"The operation ["+func+"] was successfully invoked for the MBean ["+safeObjNameStr+"].");

        if (res != null){
            add2Page("<BR>The operation returned with the value:<P>");
            add2Page(res.toString());  
        } else {
            add2Page("<BR>The operation returned with no value.");
        }

        add2Page("<P><TABLE WIDTH=100%><TR>");
        add2Page("<TD ALIGN=LEFT><A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(objNameStr)+"\">Back to "+HtmlDef.objectPageTitle+"</A></TD>");
        add2Page("<TD ALIGN=RIGHT>"+HtmlDef.LISTOFMBEAN+"</TD>");
        add2Page("</TR></TABLE>");
        
        add2Page(stopBody());
    }

    
    // --------------------------------------------------------
    // PRIVATE METHODS
    // --------------------------------------------------------

    private String getAction(String req) {
        int beg = req.indexOf("action=");
        if (beg < 0 )
            return null;
        int end = req.indexOf("?",beg);
        if (end < 0)
            return null;
        else
            return req.substring(beg+7,end);
    }

    private boolean getParams(String reqStr) {
        valueList = new ArrayList();
        typeList  = new ArrayList();
        String  typeStr;
        String  valueStr;
        String  propStr;
    
        int     index;
        boolean done       = false;
        String  propReqStr = null;
    
        while (!done) {
            // A property name separator is always &
            //
            index = reqStr.indexOf('&');
            if (index < 0) {
                // No more entry, after this one
                //
                propReqStr = reqStr;
                done       = true;
            } else {
                propReqStr = reqStr.substring(0, index);
                reqStr     = reqStr.substring(index + 1);
            }
 
            // Parse this one
            //
            index = propReqStr.indexOf('=');
      
            if (index < 0) {
                done = true;
                // We cannot continue
                //
                buildError("Syntax error in request ["+propReqStr+"]",HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                return false;
            }
      
            propStr  = propReqStr.substring(0, index);
            valueStr = propReqStr.substring(index + 1);
            typeStr  = null;
      
            if ((propStr != null) && (propStr.length() != 0)) {
                // Remove %0D in the begining of property name
                //
                //if (propStr.startsWith("%0D") || propStr.startsWith("%0d")) {
                while (propStr.startsWith("%")) {
                    propStr = propStr.substring(3);
                }
                // Substitute the %HH character on valueStr
                //
                if (valueStr != null && valueStr.length() != 0 )
                    valueStr = decodeUrl(valueStr);
                else
                    valueStr = HtmlDef.NULL;
    
                // The propertyName is build with <propertyName>%2B<value type>
                //
    
                if (logger.finerOn()) {
                    logger.finer("getParams","Parsing property name ["+propStr+"]");
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
                    logger.finer("getParams","Get Name = ["+propStr+"] "+"Type = ["+typeStr+"]"+" Value = ["+valueStr+"]");
                }
                
                // Save the pair elements
                // and filter action field
                if (!propStr.equals("action")) {
                    typeList.add(typeStr);
                    valueList.add(valueStr);
                }
            } else {
                if (logger.finerOn()) {
                    logger.finer("getParams","Got null value or property");
                }
                buildError("Syntax error in request ["+propReqStr+"]",HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                return false;
            }
        }
        if (logger.finerOn()) {
            logger.finer("getParams","Returns "+valueList.size()+" params");
        }
        return true;
    }


    // --------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------

    private ArrayList valueList = null;
    private ArrayList typeList  = null;
}
