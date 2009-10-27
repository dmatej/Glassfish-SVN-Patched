/*
 * @(#)file      HtmlArrayPage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.32
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
import java.util.Date;
import java.util.TimeZone;
import java.text.DateFormat;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MBeanInfo;
import javax.management.MBeanAttributeInfo;
import javax.management.MalformedObjectNameException;
import javax.management.AttributeNotFoundException;
import javax.management.MBeanException;
import javax.management.InstanceNotFoundException;
import javax.management.ReflectionException;
import javax.management.IntrospectionException;
import javax.management.JMRuntimeException;

import com.sun.jdmk.internal.ClassLogger;


class HtmlArrayPage
    extends HtmlPage {

    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------

    /**
     * Constructs a new HtmlArrayPage.
     */
    public HtmlArrayPage(MBeanServer f, boolean r, boolean w, HtmlAdaptorServer server) {
        super(f,r,w);
        this.server = server;
    }  


    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------

    public void buildPage(String req) {
        if (logger.finerOn()) {
            logger.finer("buildPage","Handle request ["+req+"]");
        }

        // Handle invalid request case.
        //
        int index;
        if ((index = req.indexOf("/",1)) < 0) {
            buildError("Invalid request", HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
            return;
        }
    
        String prop = req.substring(1, index);
        String objNameStr = req.substring(index , req.length());
        objNameStr = fromUrlName(objNameStr);
        String safeObjNameStr = translateNameToHtmlFormat(objNameStr);

        if (logger.finerOn()) {
            logger.finer("buildPage","Build an Array page for the MBean ["+objNameStr+"] property ["+prop+"]");
        }

        htmlPage.append(buildHead(HtmlDef.arrayPageTitle + " of " + safeObjNameStr));
        htmlPage.append(startBody(null)); // Null for color.
    
        // Get the attributes information and value from the MBean server.
        //
        Object o = null;
        ObjectName objName = null;
        MBeanAttributeInfo[] attinfo = null;
        try {
            objName = getObjectNameByObjNameStr(objNameStr);
            attinfo = mbs.getMBeanInfo(objName).getAttributes();
            o = mbs.getAttribute(objName,prop);
        } catch (IntrospectionException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("Introspection: "+safeObjNameStr,
                       HtmlDef.HTTP_ERROR_INTROSPECTION_ID + " " + HtmlDef.HTTP_ERROR_INTROSPECTION);
            return;
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("Invalid ObjectName: "+safeObjNameStr,
                       HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
            return;
        } catch (AttributeNotFoundException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("Attribute "+prop+" not found",
                       HtmlDef.HTTP_ERROR_ATTRIBUTE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_ATTRIBUTE_NOT_FOUND);
            return;
        } catch (MBeanException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("The getter of attribute "+prop+" throws the exception:<BR>"+e.getTargetException(),
                       HtmlDef.HTTP_ERROR_MBEAN_ID + " " + HtmlDef.HTTP_ERROR_MBEAN);
            return;
        } catch (InstanceNotFoundException e ) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("Instance of "+safeObjNameStr+" not found",
                       HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return;
        } catch (ReflectionException e){
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("The MBeanServer throws an ReflectionException when calling attribute["+
                       prop+"] of ["+safeObjNameStr+"] :<BR>"+
                       e.getTargetException().toString(),
                       HtmlDef.HTTP_ERROR_REFLECTION_ID + " " + HtmlDef.HTTP_ERROR_REFLECTION);
            return;
        } catch (JMRuntimeException e) {
            if (logger.finestOn()) {
                logger.finest("buildPage","Exception = "+e);
            }
            buildError("The MBeanServer throws a JMRuntimeException when calling attribute ["+prop+"] of ["+safeObjNameStr+"] :<BR>"+e,HtmlDef.HTTP_ERROR_MBEAN_ID+" "+HtmlDef.HTTP_ERROR_MBEAN);
            return;
        }

        // Find the property in the attributes list.
        //
        int k;
        for (k = attinfo.length-1; k >= 0 ; k--) {
            if (attinfo[k].getName().equals(prop)) {
                break;
            }
        }
        
        if (o != null) {
            Object[] ob = null;
            int y;
            String typeStr = getElementClass(o);
            if(typeStr == null) {
                buildError("The attribute type " + o.getClass().getName() + 
                           " is not a supported array type<BR>",
                           HtmlDef.HTTP_ERROR_MBEAN_ID+" "+
                           HtmlDef.HTTP_ERROR_MBEAN);
                return;
            } else if (typeStr.equals("int")){
                int[] tmp = (int[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            }
            else if (typeStr.equals("boolean")){
                boolean[] tmp = (boolean[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            }
            else if (typeStr.equals("char")){
                char[] tmp = (char[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            } 
            else if (typeStr.equals("double")){
                double[] tmp = (double[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            } 
            else if (typeStr.equals("float")){
                float[] tmp = (float[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            } 
            else if (typeStr.equals("long")){
                long[] tmp = (long[]) o;
                ob = new String[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=String.valueOf(tmp[y]);
            }
            else if (typeStr.equals("short")){
                short[] tmp = (short[]) o;
                ob = new Short[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=new Short(tmp[y]);
            }
            else if (typeStr.equals("byte")){
                byte[] tmp = (byte[]) o;
                ob = new Byte[tmp.length];
                for (y=tmp.length-1;y>=0;y--)
                    ob[y]=new Byte(tmp[y]);
            }
            else {
                ob = (Object[]) o;
            }
          
            String Extra = "";

            // Build the title of the page.
            //
            add2Page("<TABLE WIDTH=100%>");
            add2Page("<TR>");
            add2Page("<TD ALIGN=left><H2>"+HtmlDef.arrayPageTitle+"</H2></TD>");
            add2Page("<TD ALIGN=right VALIGN=top>["+HtmlDef.jdmkVersion+"]</TD>");
            add2Page("</TR></TABLE>");
            
            // Build top of page.
            //
            add2Page("<UL type=disc><LI><B>MBean Name:</B> " + safeObjNameStr);
            add2Page("<LI><B>MBean Attribute:</B> "+prop);
            add2Page("<LI><B>Array of:</B> "+typeStr);
            add2Page("</UL>");
        
            add2Page("<TABLE WIDTH=100%><TR>");
            add2Page("<TD ALIGN=LEFT><A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(objNameStr)+"\">Back to "+HtmlDef.objectPageTitle+"</A></TD>");
            add2Page("<TD ALIGN=RIGHT>"+HtmlDef.LISTOFMBEAN+"</TD>");
            add2Page("</TR></TABLE>");
            add2Page("<HR>");
            // WRITE ONLY    
            if (attinfo[k].isWritable() && !attinfo[k].isReadable()) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","The property [Name="+prop+", Type="+typeStr+"] is WR");
                }
      
                add2Page("<FORM ACTION="+HtmlDef.SETFORM+toUrlName(objNameStr)+" METHOD=GET>");
                add2Page("<TABLE ALIGN=center BORDER=1 WIDTH=100% CELLPADDING=3>");
                add2Page("<TR>");
                add2Page("<TH WIDTH=10%> Element at </TH>");
                add2Page("<TH WIDTH=7%> Access </TH>");
                add2Page("<TH WIDTH=35%> Value </TH>");
                add2Page("</TR>");
        
                for (int i = 0; i < ob.length; i++) {
                    add2Page("<TR>");
                    add2Page("<TD ALIGN=center>"+i+"</TD><TD ALIGN=center>WO</TD>");
                    if (ob[i] == null) {
                        add2Page("<TD>null</TD>");
                    } else {
                        if (typeStr.endsWith("Boolean") ||
                            typeStr.endsWith("boolean")) {
                            // We can CHECKED one.
                            //
                            add2Page("<TD>");
                            add2Page(boolToHtml(prop+"["+i+"]",typeStr,ob[i].toString(),false));
                            add2Page("</TD>");
                        } 
                        else {
                            htmlPage.append("<TD><INPUT TYPE=\"password\" NAME=\""+prop+"["+i+"]+"+typeStr+"\" ");
                            add2Page("SIZE=34%></TD>");
                        }
                    }
                    add2Page("</TR>");
                }
                add2Page("</TABLE>");
                add2Page("<ALIGN=center>");
                add2Page("<INPUT TYPE=submit VALUE=\""+HtmlDef.setChangesBut+"\"> ");
            }
            // READ ONLY
            else if (attinfo[k].isReadable() && ! attinfo[k].isWritable()) {
                if (logger.finerOn()){
                    logger.finer("buildPage","The property [Name="+prop+", Type="+typeStr+"] is RO");
                }
                add2Page("<A NAME=\"Top\"></A>");
                add2Page("<TABLE ALIGN=center BORDER=1 CELLPADDING=3>");
                add2Page("<TR>");
                add2Page("<TH WIDTH=10%> Element at </TH>");
                add2Page("<TH WIDTH=7%> Access </TH>");
                add2Page("<TH WIDTH=35%> Value </TH>");
                add2Page("</TR>");
                
                for (int i = 0; i < ob.length; i++) {
                    add2Page("<TR>");
                    add2Page("<TD ALIGN=center>"+i+"</TD><TD ALIGN=center>RO</TD>");
                    if (ob[i] == null ) {
                        htmlPage.append("<TD>null</TD>");
                    } 
                    else if (typeStr.equals("javax.management.ObjectName")) {
                        add2Page("<TD><A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(ob[i].toString())+"\">"+
                                 ob[i].toString()+"</A></TD>");
                    }     
                    else if (typeStr.endsWith("Date")){     
                        add2Page("<TD>");
                        add2Page(convertDate((Date)ob[i]));
                        add2Page("</TD>");
                    }           
                    else if (!checkType(typeStr)) {
                        Extra += "<P><A NAME=\"Element"+i+"\"> Element "+i +"</A> - <A HREF=\"#Top\">Go back to main table</A>";
                        Extra += buildEleTable(ob[i]);
                        add2Page("<TD><I>Type Not Supported</I>: ["+ translateNameToHtmlFormat(ob[i].toString()) +"] <A HREF=\"#Element"+i+"\">View the value</A></TD>");
                    }
                    else {
                        add2Page("<TD><PRE>"+translateNameToHtmlFormat(ob[i].toString())+"</PRE></TD>");
                    }
                    add2Page("</TR>");
                }
                add2Page("</TABLE>");
                add2Page(Extra);
            }
            //READ WRITE
            else if (attinfo[k].isReadable() && attinfo[k].isWritable()) {
                if (logger.finerOn()) {
                    logger.finer("buildPage","The property [Name="+prop+", Type="+typeStr+"] is RW");
                }      
                add2Page("<FORM ACTION="+HtmlDef.SETFORM+toUrlName(objNameStr) + " METHOD=GET>" );
                add2Page("<TABLE ALIGN=center BORDER=1 CELLPADDING=3>");
                add2Page("<TR>");
                add2Page("<TH WIDTH=10%> Element at </TH>");
                add2Page("<TH WIDTH=7%> Access </TH>");
                add2Page("<TH WIDTH=35%> Value </TH>");
                add2Page("</TR>");

                for (int i = 0; i < ob.length; i++) {
                    add2Page("<TR>");
                    add2Page("<TD ALIGN=center>"+i+"</TD><TD ALIGN=center>RW</TD>");

                    if (typeStr.endsWith("Boolean") || typeStr.endsWith("boolean")) {
                        // We can CHECKED one.
                        //
                        add2Page("<TD>");
                        add2Page(boolToHtml(prop+"["+i+"]",typeStr,(ob[i] != null?ob[i].toString():"null"),(ob[i]==null?false:true)));
                        add2Page("</TD>");
                    } else if (typeStr.equals("javax.management.ObjectName")) {
                        htmlPage.append("<TD><INPUT TYPE=\"text\" NAME=\""+prop+"["+i+"]+"+typeStr+"\" ");
                        htmlPage.append("VALUE= \""+(ob[i] != null?ob[i].toString():"")+"\" ");
                        add2Page("SIZE="+(ob[i] != null? ob[i].toString().length() : 20)+">");
                        add2Page("<A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(ob[i] != null ?ob[i].toString():"")+"\">V</A></TD>");
                    } else if (typeStr.endsWith("Date")){       
                        htmlPage.append("<TD><INPUT TYPE=\"text\" NAME=\""+prop+"["+i+"]+"+typeStr+"\" ");
                        htmlPage.append("VALUE= \""+convertDate((Date)ob[i])+"\" ");
                        add2Page("SIZE="+(ob[i] != null?ob[i].toString().length():20)+"></TD>");
                    } else if (!checkType(typeStr)) {
                        Extra += "<A NAME=\"Element"+i+"\"> Element "+i+"</A> - <A HREF=\"#Top\">Go back to main table</A>";
                        Extra += buildEleTable(ob[i]);
                        add2Page("<TD><I>Type Not Supported</I>: ["+ translateNameToHtmlFormat(ob[i].toString()) +"] <A HREF=\"#Element"+i+"\">View the value</A></TD>");
                    } else {
                        htmlPage.append("<TD><INPUT TYPE=\"text\" NAME=\""+prop+"["+i+"]+"+typeStr+"\" ");
                        htmlPage.append("VALUE= \""+translateNameToHtmlFormat(ob[i] != null ? ob[i].toString(): "")+"\" ");
                        add2Page("SIZE=34%></TD>");
                    }
                    add2Page("</TR>");
                }
                add2Page("</TABLE>");
                if (Extra.equals("")) {
                    add2Page("<INPUT TYPE=submit VALUE=\""+HtmlDef.setChangesBut+"\"> ");
                } else {
                    add2Page(Extra);
                }
                add2Page("</FORM>");
            }    
        } else {
            if (logger.finerOn()) {
                logger.finer("buildPage","The property ["+prop+"] is a null");
            }
            add2Page("<p> Attribute "+prop+" is null");
        }

        add2Page(stopBody());
    }


    // --------------------------------------------------------
    // PRIVATE METHODS
    // --------------------------------------------------------

    private String buildEleTable(Object obj) {
        if (obj == null) {
            return "Null object";
        }

        Class  c     = obj.getClass();
        String cName = c.getName();

        if (logger.finerOn()) {
            logger.finer("buildEleTable","Build a table for the element [Name= "+obj+"Classname="+cName+"]");
        }
    
        // Let's start the real work.
        //
        Method[] methods;
        Class[] parameterType;
        methods = c.getMethods();
    
        // Look thru the defined methods to get the
        // associated properties.
        // For all getters and setters, we need to save:
        //   - The property name.
        //   - The propery type.
        //   - A flag for read/write
        //   - The string representation to display this property
        //
        String propertyName[] = new String[methods.length];
        String propertyType[] = new String[methods.length];
        String propertyView[] = new String[methods.length];
    
        // Keep the max length property size for the
        // display.
        //
        int maxViewLength     = 0;
        int nbGoodProperties  = 0;
    
        // Display on the page if we found setter we
        // cannot manage because of the property type is
        // not easely conversible from a String.
        //
        boolean foundBadSetter = false;

        for (int i = 0; i < methods.length; i++) {
            Class  returnType;
            String mName;
            String declName;
            boolean flat = true;
      
            mName          = methods[i].getName();
            declName       = methods[i].getDeclaringClass().getName();
            returnType     = methods[i].getReturnType();
            parameterType  = methods[i].getParameterTypes();
      
            if (logger.finerOn()) {
                logger.finer("buildEleTable","Analyze element ["+obj+"] method ["+returnType+" "+mName+"("+parameterType.length+" param)]");
            }

            if (mName.startsWith("get")) {
                // Validate the bean design pattern ...
                // And we have to validate the return type too. Must be a constructed type.
                //
                if (parameterType.length > 0) {
                    propertyName[i] = null;
                    continue;
                }
                propertyName[i] = new String(mName.substring(3));
                propertyType[i] = usualType(new String(returnType.getName()));
    
                // Retrieve the value for this property.
                //
                Object o = null;
                boolean reqDone = false;
                
                try {
                    // Check if it's an array.
                    //
                    if (isArrayType(propertyType[i])) {
                        if (logger.finerOn()) {
                            logger.finer("buildEleTable","Found array [Name="+propertyName[i]+", Type= "+propertyType[i]+"]");
                        }
                        o = "values of " + propertyName[i];
                    } else {
                        if (logger.finerOn()) {
                            logger.finer("buildEleTable","Call the method [Name="+methods[i]+"] for Property [Name="+propertyName[i]+"]");
                        }
                        o = methods[i].invoke(obj, (Object[])null);
                    }
                    reqDone = true;
                } catch (InvocationTargetException e) {
                    if (logger.finestOn()) {
                        logger.finest("buildEleTable",
                              "Cannot get value for propertyName[" + i + "] = " + 
                              propertyName[i] + " Got exception:\n" + e.toString());
                        logger.finest("EXCEPTION",e.toString());
                    }
                } catch (IllegalAccessException e) {
                    if (logger.finestOn()) {
                        logger.finest("buildEleTable",
                              "Cannot get value for propertyName[" + i + "] = " + 
                              propertyName[i] + " Got exception:\n" + e.toString());
                        logger.finest("EXCEPTION",e.toString());
                    }
                } catch (IllegalArgumentException e) {
                    if (logger.finestOn()) {
                        logger.finest("buildEleTable",
                              "Cannot get value for propertyName[" + i + "] = " + 
                              propertyName[i] + " Got exception:\n" + e.toString());
                        logger.finest("EXCEPTION",e.toString());
                    }
                }
        
                // Remove a non standard property from the list
                //
                if (!reqDone) {
                    if (logger.finerOn()) {
                        logger.finer("buildEleTable","Invocation error: Remove property ["+propertyName[i]+"]");
                    }
                    propertyName[i] = null;
                } else {
                    nbGoodProperties++;
                    // Get the string associated to the result
                    //
                    if (o == null) {
                        propertyView[i] = "";
                    } else {
                        propertyView[i] = o.toString();
                        if (propertyView[i].length() > maxViewLength) {
                            maxViewLength = propertyView[i].length();
                        }
                    }
                }
            } else {
                propertyName[i] = null;
                continue;
            }
        }
    
        // Build the HTML Page
        //
        StringBuffer htmlProperty = new StringBuffer();
    
        if (nbGoodProperties > 0) {
            boolean hasInput = false;
      
            // Build a table.
            // Table format:
            // 
            // +----------+------+--------+------+
            // | Property | Type | Access | Value|
            // +----------+------+--------+------+
            // ...
            // +----------+------+--------+------+
            //
            htmlProperty.append("<TABLE ALIGN=center BORDER=1>" + HtmlDef.PF +
                                "<TR>" + HtmlDef.PF +
                                "<TH> Property Name </TH>" + HtmlDef.PF +
                                "<TH> Type </TH>" + HtmlDef.PF +
                                //"<TH> Access </TH>" + HtmlDef.PF +
                                "<TH> Value </TH>" + HtmlDef.PF +
                                "</TR>" + HtmlDef.PF);
      
            for (int i = 0; i < methods.length; i++) {
                if (propertyName[i] == null) {
                    continue;
                }
                // Set this entry
                //
                htmlProperty.append("<TR>" + HtmlDef.PF +
                                    "<TD><B>" + propertyName[i] + "</B></TD>" +
                                    "<TD>" + propertyType[i] + "</TD>" + HtmlDef.PF);
              
                // htmlProperty.append("<TD ALIGN=center> RO </TD>" + HtmlDef.PF);
                if (propertyView[i] != null) {
                    if (isArrayType(propertyType[i])) {
                        if (logger.finerOn()) {
                            logger.finer("buildEleTable","Build link to array page.");
                        }
                        htmlProperty.append("<TD>Not displayable</TD>" + HtmlDef.PF);
                    } else { 
                        htmlProperty.append("<TD>" + propertyView[i] + "</TD>" + HtmlDef.PF);
                    }
                } else {
                    htmlProperty.append("<TD> ?? </TD>" + HtmlDef.PF);
                }
                htmlProperty.append("</TR>" + HtmlDef.PF);
            }
            htmlProperty.append("</TABLE>" + HtmlDef.PF);
        }    
        return (htmlProperty.toString());
    }
  
    private String getElementClass(Object o) {    
        String str = usualType(o.getClass().getName());
        if (!str.endsWith("[]"))
            return null;
    
        return (str.substring(0,str.length() - 2));
    }
  
    private String convertDate(Date d){
        if (d == null)
            return "";
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);
        //java BUG : the TZ of the DateFormat is not set correctly with the method getDateTimeInstance.
        // By default, the DateFormat use the TZ of the system.
        df.setTimeZone(TimeZone.getDefault());
        return (df.format((Date)d));
    }

    // --------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------

    private HtmlAdaptorServer server = null;
}
