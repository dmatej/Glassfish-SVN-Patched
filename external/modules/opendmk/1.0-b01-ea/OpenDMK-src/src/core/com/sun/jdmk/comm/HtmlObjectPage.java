/*
 * @(#)file      HtmlObjectPage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.44
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
import java.util.Set;
import java.util.ArrayList;
import java.util.Enumeration;
import java.net.URLEncoder;
import java.lang.reflect.Constructor;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.MBeanInfo;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MalformedObjectNameException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.ReflectionException;
import javax.management.IntrospectionException;
import javax.management.JMRuntimeException;
import javax.management.ObjectName;
import javax.management.AttributeList;
import javax.management.Attribute; 
import javax.management.AttributeNotFoundException;
import javax.management.MBeanServerFactory;
import javax.management.loading.ClassLoaderRepository;


// jmx RI import
//
import com.sun.jdmk.Enumerated;

class HtmlObjectPage extends HtmlPage {

    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------
 
    /**
     * Constructs a new HtmlObjectPage.
     */
    public HtmlObjectPage(MBeanServer f, boolean r, boolean w, HtmlAdaptorServer server) {
        super(f,r,w);
        this.server = server;
    }


    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------

    public void buildPage(String req) {
        if (logger.finerOn()) {
            logger.finer("buildPage","Handle request = "+req);
        }

        String objNameStr = req;
        int disp = req.indexOf("?");

        if (disp >= 0){
            objNameStr = req.substring(0,disp);
        }
        objNameStr = fromUrlName(objNameStr);

        if (!analyseObj(objNameStr)) {
            return;
        }

        // Start the object(MBean) page.
        //
        if (!startPage(objNameStr)) {
            return;
        }

        // Build the attribute part of the object(MBean) page.
        //        
        buildAttributes(objNameStr);

        // Build the operations part of the object(MBean) page.
        //
        buildOperations();

        // End the object(MBean) page.
        //
        endPage();
    }

    /**
     * Ends the object(MBean) page.
     */
    public void endPage() {
        if (logger.finerOn()) {
            logger.finer("endPage","End the object(MBean) page.");
        }
        add2Page(stopBody());
    }

    /**
     * Starts the object(MBean) page.
     */
    public boolean startPage(String objNameStr) {
        if (logger.finerOn()) {
            logger.finer("startPage","Start the object(MBean) page for = "+objNameStr);
        }
	
	String safeObjNameStr = translateNameToHtmlFormat(objNameStr);

        if (meta != null){
            htmlPage.append(buildHeadMeta(HtmlDef.objectPageTitle +" of "+ safeObjNameStr, meta));
        } else {
            htmlPage.append(buildHead(HtmlDef.objectPageTitle +" of "+ safeObjNameStr));
        }
        htmlPage.append(startBody(null)); // Null for color.

        // Build the title of the page.
        //
        add2Page("<TABLE WIDTH=100%>");
        add2Page("<TR>");
        add2Page("<TD ALIGN=left><H2>"+HtmlDef.objectPageTitle+"</H2></TD>");
        add2Page("<TD ALIGN=right VALIGN=top>["+HtmlDef.jdmkVersion+"]</TD>");
        add2Page("</TR></TABLE>");

        // Build the MBean object name and class information
        //
        add2Page("<UL type=disc><LI><B>MBean Name:</B> " + safeObjNameStr);
        add2Page("<LI><B>MBean Java Class:</B> "+manipulatedObj.getClassName());
        add2Page("</UL>");

        add2Page("<TABLE WIDTH=100%><TR>");
        add2Page("<TD ALIGN=LEFT>"+HtmlDef.LISTOFMBEAN+"</TD>");

        add2Page("<TD ALIGN=CENTER>");
        add2Page("<FORM ACTION=" + HtmlDef.AUTOREFRESH + toUrlName(objNameStr) + " METHOD=GET>");
        add2Page("Reload Period in seconds:<BR><INPUT type=text name=period value="+autoRefresh+" SIZE=2>"+
                 "<INPUT TYPE=submit VALUE=\"Reload\"></FORM></TD>");


        add2Page("<TD ALIGN=RIGHT><BR><FORM ACTION=\"" + 
                 HtmlDef.ADMIN+
                 HtmlDef.ADMIN_OBJECT+
                 HtmlDef.ADMIN_QUEST2+
                 "&keysName="+URLEncoder.encode(manipulatedObjName.getKeyPropertyListString())+
                 "&domainName="+URLEncoder.encode(manipulatedObjName.getDomain())+
                 "&className="+URLEncoder.encode(manipulatedObj.getClassName())+
                 "&action="+HtmlDef.actionDelete + "&\" METHOD=GET>");
        add2Page("<FONT SIZE=-1><INPUT type=submit VALUE=\""+HtmlDef.actionDelete+"\" ></FONT></FORM></TD>"); 
        add2Page("</TR></TABLE>");
        return true;
    }

    public boolean setObjectValue(String objNameStr, String reqStr) {

        if (logger.finerOn()) {
            logger.finer("setObjectValue","[name="+objNameStr+", request="+reqStr+"]");
        }


        StringBuffer errBuf = new StringBuffer();
        boolean      error  = false;

        objNameStr = fromUrlName(objNameStr);
	String safeObjNameStr = translateNameToHtmlFormat(objNameStr);

        // Parse the request and build 3 lists
        // one for the property name.
        // one for the string value.
        // one for the type name of this value.
        //
        if (! reqStr.startsWith("?")) {
            buildError("Incorrect request form, must start with ?",
		       HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
            return false;
        }

        reqStr = reqStr.substring(1);

        int index;
        int indexValue;
        ArrayList anArray = null;           ;
        AttributeList attlist = new AttributeList();

        String  propReqStr  = reqStr;
        boolean done = false;
        String  propStr = null;
        String  typeStr = null;
        String  valueStr;
        String ind = null;

        try {
            manipulatedObjName = getObjectNameByObjNameStr(objNameStr);
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("setObjectValue","Exception = "+e);
            }
            buildError("The format of the string ["+ safeObjNameStr +"] does not correspond to a valid ObjectName <P>"+ e.toString() +"<P>",
		       HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID +" "+ HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
            return false;
        }

        while (!done) {
            // A property name separator is always "&".
            //
            index = reqStr.indexOf('&');
            if (index < 0) {
                // No more entries, after this one
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
                buildError("Syntax error in request ["+propReqStr+"]",
			   HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                return false;
            }

            propStr  = propReqStr.substring(0, index);
            valueStr = propReqStr.substring(index + 1);

            if ((valueStr != null) && (propStr != null) &&
                (valueStr.length() != 0) && (propStr.length() != 0)) {
                // Remove %0D in the begining of property name.
                //
                //if (propStr.startsWith("%0D") || propStr.startsWith("%0d")) {
                while (propStr.startsWith("%")) {
                    propStr = propStr.substring(3);
                }
                // Substitute the %HH character on valueStr.
                //
                valueStr = decodeUrl(valueStr);

                // The propertyName is built with <propertyName>%2B<value type>.
                //

                if (logger.finerOn()) {
                    logger.finer("setObjectValue","Parsing property name ["+propStr+"]");
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

                // This is an indexed property, format AnArray%5B0%5D.
                //
                ind = ""; 
                if (propStr.indexOf("%5B") > 0) {  
                    ind = propStr.substring(propStr.indexOf("%5B")+3,propStr.indexOf("%5D"));
                    propStr = propStr.substring(0, propStr.indexOf("%5B"));
                } 
                else if(propStr.indexOf("%5b") > 0) {
                    ind = propStr.substring(propStr.indexOf("%5b")+3,propStr.indexOf("%5d"));
                    propStr = propStr.substring(0, propStr.indexOf("%5b"));
                }

                if (logger.finerOn()) {
                    logger.finer("setObjectValue",
			  "Get Name = ["+propStr+"] "+(ind.length()>0?"["+ind+"] ":" ")+"Type = ["+typeStr+"]"+" Value = ["+valueStr+"]");
                }


		// Now let's build the value for this attribute
		//
                Object attvalue = null;

		// Case for an Enumerated
		//
                if (propStr.startsWith("Enumerated%29")) {
                    propStr = propStr.substring(13);
		    // do not set this Enumerated's value if it is the dummy display "---" for WO Enumerated
		    if (!valueStr.equals(HtmlDef.HTML_WO_DEFAULT)) {
			try {
			    // get info on this attribute to know if it is readable
			    MBeanAttributeInfo[] attrInfos = mbs.getMBeanInfo(manipulatedObjName).getAttributes();
			    int i = 0;
			    boolean found = false;
			    while ( (i < attrInfos.length) && ( !(found=attrInfos[i].getName().equals(propStr))) ) {
				i++ ;
			    }
			    // Get the class for this attribute
			    Class ocl;
			    if (found == true) {
				if (attrInfos[i].isReadable()) {
				    ocl = mbs.getAttribute(manipulatedObjName, propStr).getClass();
				} else {
				    ocl = loadClass(typeStr);
				}
				if (logger.finerOn()) {
				    logger.finer("setObjectValue","Got Class for Enumerated attribute["+ propStr +"] is ["+ ocl.getName() +"]");
				}
			    } else {
				throw new AttributeNotFoundException("Could not find attribute ["+ propStr +
								     "] in MBean ["+ manipulatedObjName +"]");
			    }

			    Class sc = ocl.getSuperclass();
			    if ( (sc != null) && (com.sun.jdmk.Enumerated.class.isAssignableFrom(sc)) ) {
				Constructor[] cons = ocl.getDeclaredConstructors();
				for (int consi = 0 ;consi <cons.length;consi++) {
				    Class[] cl = cons[consi].getParameterTypes();
				    if (cl.length == 1 
					&& cl[0].getName().endsWith("String")) {
					if (logger.finerOn()) {
					    logger.finer("setObjectValue","Found constructor for Enumerated = "+ocl.getName());
					}
					Object[] param = new Object[1];
					param[0] = valueStr;
					attvalue = cons[consi].newInstance(param);
				    }
				}
			    } else {
				buildError("Cannot get constructor of "+typeStr+" for setting "+propStr+": Invalid Enumerated.", 
					   HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID+" "+HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE);
				return false;
			    }
			} catch (Exception e) {
			    if (logger.finestOn()) {
				logger.finest("setObjectValue","Exception = "+e);
			    }
			    buildError("Cannot get constructor of "+ typeStr +" for setting "+ propStr +" of MBean "+ 
				       manipulatedObjName.toString()+"<P>The following exception has been produced:<BR>"+e.toString()+"<P>",
				       HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID+" "+HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE);
			    return false;
			}
		    }
                } else {
                    // Convert to the new type if needed.
                    //
                    attvalue = stringToObject(typeStr,valueStr);
                }

		// Value will be added to list to be set only if not null
                if (attvalue != null) {
		    if (ind.length() > 0) {
			// This is an array,
			// treat each element as value of an array.
			//
			if (anArray == null) {
			    anArray = new ArrayList();
			}
			try {
			    indexValue = java.lang.Integer.parseInt(ind);
			} catch (NumberFormatException e) {
			    if (logger.finestOn()) {
				logger.finest("setObjectValue","Exception = "+e);
			    }
			    buildError("Invalid Index value ["+ind+"] for property ["+propStr+"]<P>",
				       HtmlDef.HTTP_ERROR_BAD_REQUEST_ID+" "+HtmlDef.HTTP_ERROR_BAD_REQUEST);
			    return false;
			}                        
			anArray.add(indexValue, attvalue);
			if (logger.finerOn()) {
			    logger.finer("setObjectValue","Add element "+indexValue+" to arrayList "+propStr+" type = "+attvalue.getClass().getName());
			}
		    } else {
			// This is not an array,
			// treat values as independant attributes.
			//
			Attribute myAttr = new Attribute(propStr, attvalue);
			attlist.add(myAttr);

			indexValue = -1;
			if (logger.finerOn()) {
			    logger.finer("setObjectValue",
				  "added [name="+ myAttr.getName() +",value="+ myAttr.getValue() +",type="+ typeStr +
				  "] to list of attributes to set");
			}
		    }
		}
            } else {
                if (logger.finerOn()) {
                    logger.finer("setObjectValue","Got null value or property");
                }
            }
        } // END while (!done) LOOP

        if (anArray != null) {
            attlist.add(new Attribute(propStr,getArray(typeStr,anArray)));
        }

        // Call MBeanServer to set the values.
        //
        AttributeList resAttList = null;
        try {
            resAttList = mbs.setAttributes(manipulatedObjName,attlist);
	    if (logger.finerOn()) {
		logger.finer("setObjectValue","Invoked setAttributes() on "+ manipulatedObjName +". Checking result.");
	    }
        } catch (InstanceNotFoundException e) {
            if (logger.finestOn()) {
                logger.finest("setObjectValue","Exception = "+e);
            }
            buildError("Setting of attributes has produced an exception <P>"+e.toString()+"<P>",
		       HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID+" "+HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return false;
        } catch (ReflectionException e) {
            if (logger.finestOn()) {
                logger.finest("setObjectValue","Exception = "+e);
            }
            buildError("Setting of attributes has produced an exception <P>" +e.toString() + "<P>",
		       HtmlDef.HTTP_ERROR_REFLECTION_ID + " " + HtmlDef.HTTP_ERROR_REFLECTION);
            return false;
        } catch (JMRuntimeException e) {
            if (logger.finestOn()) {
                logger.finest("setObjectValue","Exception = "+e);
            }
            buildError("The MBeanServer throws a JMRuntimeException when setting attributes for ["+ safeObjNameStr +"] :<BR>"+e,
		       HtmlDef.HTTP_ERROR_MBEAN_ID+" "+HtmlDef.HTTP_ERROR_MBEAN);
            return false;
        } 

        if (attlist.size() != resAttList.size()) {
            if (logger.finerOn()) {
                logger.finer("setObjectValue","Initial Number of attribute to set = "+attlist.size()+" and effectively set = "+resAttList.size());
            }
            int noset = attlist.size()-resAttList.size();
            String mess = noset+"/"+attlist.size()+" attribute(s) were not set:<BR><UL>";
            int k,l;
            for (k = attlist.size()-1 ; k >=0  ; k--){
                for ( l = resAttList.size()-1 ; l >= 0 ; l--){
                    if (((Attribute)attlist.get(k)).getName().equals(((Attribute)resAttList.get(l)).getName())) 
                        break;
                }
                if (l == -1) 
                    mess += "<LI>"+((Attribute)(attlist.get(k))).getName();
            }
            mess += "</UL><HR>";
            buildError(mess, HtmlDef.HTTP_ERROR_SET_ATTRIBUTES_ID + " " + HtmlDef.HTTP_ERROR_SET_ATTRIBUTES);
            return false;
        }
        return true;
    }

    public String getMeta(){
        return (meta);
    }

    public void buildMeta(String objName) {
        String objNameStr = objName.substring(0,objName.indexOf("?"));

        try {
            autoRefresh = Integer.parseInt(objName.substring(objName.indexOf("period=")+7));
        } catch (NumberFormatException e){
            autoRefresh = minAutoRefresh;
        }
        if (autoRefresh == 0) {
            meta = " ";
        } else {
            if (autoRefresh < minAutoRefresh) {
                autoRefresh = minAutoRefresh;
            }
            meta = "<META HTTP-EQUIV=REFRESH CONTENT=\"";
            meta += autoRefresh+"; URL="+HtmlDef.AUTOREFRESH+objName+"\">";
        }
    }


    // --------------------------------------------------------
    // PROTECTED METHODS
    // --------------------------------------------------------

    /** 
     * Returns the Html representation of an instance of com.sun.jdmk.Enumerated,
     * depending on the access rights of this attribute.
     */
    protected String enumToHtml(Enumerated o, MBeanAttributeInfo attrInfo) {

	if (attrInfo == null) {
	    if (logger.finestOn()) {
                logger.finest("enumToHtml","Got null MBeanAttributeInfo parameter. Returning empty string");
            }
	    return "";
	}

	String attrName = attrInfo.getName();
	String attrType = attrInfo.getType();
	boolean r = attrInfo.isReadable();
	boolean w = attrInfo.isWritable();

	// o can be null for a Write-Only attribute,
	// so we need to handle this case:
	//
	if (o == null) {
	    if (logger.finestOn()) {
                logger.finest("enumToHtml","Got null Enumerated object for ["+ attrName +"]. attrInfo says [READ="+ r +",WRITE="+ w +"]");
            }
	    // make sure it is considered not readable
	    r = false;
	    if (w == true) {
		// try to build an instance of this attribute to display to the user selectable values
		try {
		    final Class cl = loadClass(attrType);
		    o = (Enumerated) cl.newInstance() ;
		} catch (Exception e) {
		    // could not instantiate: won't be able to build list of selectable values
		    w = false;
		    if (logger.finestOn()) {
			logger.finest("enumToHtml","Could not create instance of ["+ attrType +"] : "+ e);
		    }
		}
	    }
	}

        StringBuffer html = new StringBuffer(100);
        String ele = "";

	if (w == true) {
	    // if writable, we display a select box to allow to change the value
	    html.append("<SELECT NAME=\"") ;
	    html.append("(Enumerated)"+ attrName +"+"+ attrType +"\" >");
	    if (r == false) {
		html.append("<OPTION SELECTED>"+ HtmlDef.HTML_WO_DEFAULT);
	    }
	    for (Enumeration e = o.valueStrings(); e.hasMoreElements(); ) {
		ele = (String)e.nextElement();
		html.append("<OPTION");
		if ( (r == true) && (o.toString() != null) && (o.toString().equals(ele)) ) {
		    // if it is also readable we display the current value as the one initially selected     
		    html.append(" SELECTED ");
		}
		html.append( ">" + ele);
	    }
	    html.append( "</SELECT>" );
	} else if (r == true) {
	    // read-only case: we display only the current string value
	    html.append("Enumerated"+ o.toString()); // "Enumerated" tag will be removed by buildAttributes method
	} else {
	    // if it happens there has been a bug: debug msg + html will be returned empty
	    if (logger.finestOn()) {
                logger.finest("enumToHtml","Attribute is unreadable and unwritable. Returning empty string");
            }
	}
        return html.toString();
    }


    /**
     * Replace inverses of HtmlPage.
     */
    protected void inverseS(String[] a ,int lo, int hi){
        MBeanAttributeInfo ti = attinfo[lo];
        attinfo[lo] = attinfo[hi];
        attinfo[hi] = ti;        
        String t = a[lo];
        a[lo] = a[hi];
        a[hi] = t;
    }


    // --------------------------------------------------------
    // PRIVATE METHODS
    // --------------------------------------------------------

    /**
     * Gets the MBean information.
     */
    private boolean analyseObj(String objNameStr) {
        if (logger.finerOn()) {
            logger.finer("analyseObj","Collect MBean information for = "+objNameStr);
        }

	String safeObjNameStr = translateNameToHtmlFormat(objNameStr);

        // Verify the given object name for an MBean.
        //
        try {
            manipulatedObjName = getObjectNameByObjNameStr(objNameStr);
        } catch (MalformedObjectNameException e) {
            if (logger.finestOn()) {
                logger.finest("analyseObj","Exception = "+e);
            }
            buildError("The format of the string ["+safeObjNameStr+"] does not correspond to a valid ObjectName <P>" +e.toString() + "<P>",
		       HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
            return false;
        }

        // Get the m-bean information.
        //
        if (manipulatedObjName != null) {
            try {
                manipulatedObj = mbs.getMBeanInfo(manipulatedObjName);
            } catch(InstanceNotFoundException e) {
                if (logger.finestOn()) {
                    logger.finest("analyseObj","Exception = "+e);
                }
                buildError("Unable to get MBeanInfo for ["+safeObjNameStr+"]", 
			   HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
                return false;
            } catch(IntrospectionException e) {
                if (logger.finestOn()) {
                    logger.finest("analyseObj","Exception = "+e);
                }
                buildError("Unable to get MBeanInfo for ["+safeObjNameStr+"]", 
			   HtmlDef.HTTP_ERROR_INTROSPECTION_ID + " " + HtmlDef.HTTP_ERROR_INTROSPECTION);
                return false;
            } catch (ReflectionException e) {
                if (logger.finestOn()) {
                    logger.finest("analyseObj","Exception = "+e);
                }
                buildError("Unable to get MBeanInfo for ["+safeObjNameStr+"]", 
			   HtmlDef.HTTP_ERROR_REFLECTION_ID + " " + HtmlDef.HTTP_ERROR_REFLECTION);
                return false;
            } 
        }

        // No MBean information, return with an error.
        //
        if (manipulatedObj == null) {
            buildError("Unable to get MBeanInfo for ["+manipulatedObjName+"]", 
		       HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return false;
        }

	// Get attributes info
        //
        attinfo = checkedAttributeInfo(manipulatedObj.getAttributes());
        propertyName = new String[attinfo.length];
        propertyView = new String[attinfo.length];
        for (int j = propertyName.length-1; j >=0 ; j--) {
            propertyName[j] = attinfo[j].getName();
        }
        // Sort the attributes by name.
        quicksort(propertyName,0,propertyName.length-1);

        // Build the list of displayable attributes string values
        //
        for (int j = propertyName.length-1; j >=0 ; j--) {
	    // call getStringValue even for WO attr, as it returns what we want to display
            propertyView[j] = getStringValue(manipulatedObjName, attinfo[j]);
            if (logger.finerOn()) {
                logger.finer("analyseObj","MBean attribute: "+propertyName[j] +"=" +propertyView[j]);
            }
        }
        return true;
    }

    private MBeanAttributeInfo[] checkedAttributeInfo(MBeanAttributeInfo[] orig) {
	if(orig == null)
	    return null;
	
	MBeanAttributeInfo[] attr = new MBeanAttributeInfo[orig.length];
	for(int i = 0; i < attr.length; i++) {
	    if(orig[i]!= null)
		attr[i] = orig[i];
	    else
		attr[i] = new MBeanAttributeInfo("null_MBeanAttributeInfo", 
						 null,
						 "The attribute info at index"+
						 "[" + i + "] is null. Please"+
						 " check the MBeanInfo.",
						 true,
						 false,
						 false);
	}
	
	return attr;
    }
    
    private MBeanOperationInfo[] checkedOperationInfo(MBeanOperationInfo[] orig) {
	if(orig == null)
	    return null;
	
	MBeanOperationInfo[] oper = new MBeanOperationInfo[orig.length];
	for(int i = 0; i < oper.length; i++) {
	    if(orig[i]!= null)
		oper[i] = orig[i];
	    else
		oper[i] = new MBeanOperationInfo("null_MBeanOperationInfo",
						 "The operation info at index"+
						 "[" + i + "] is null. Please"+
						 " check the MBeanInfo.",
						 null,
						 null,
						 MBeanOperationInfo.INFO);
	}
	
	return oper;
    }
    
    /**
     * Builds the attribute part of the object(MBean) page.
     */
    private void buildAttributes(String objNameStr) {
        if (logger.finerOn()) {
            logger.finer("buildAttributes","Build attribute part for = "+objNameStr);
        }

        // Add the MBean description to the page,
        // if one is provided.
        //
        String desc = manipulatedObj.getDescription();
        if (desc != null && desc.length() > 0 ) {
            add2Page("<HR><H3>MBean description:</H3><P>"+desc+"<P>");
        }

        // Build a attribute table.
        // Table format:
        // 
        // +----------+------+--------+-------+
        // | Name | Type | Access | Value |
        // +----------+------+--------+-------+
        // ...
        // +----------+------+--------+------+
        //
        // Then a apply button for sending the modifications.
        //
        add2Page("<HR><H3>List of MBean attributes:</H3>"+HtmlDef.PF);

        if (logger.finerOn()) {
            logger.finer("buildAttributes","Found "+attinfo.length+" attributes(s)");
        }

        /* 
         * No attributes to process.
         * Return with an appropriate message in the html page.
         */
        if (attinfo.length == 0) {
            add2Page("<I>No Attributes</I><P>");
            return;
        }

        /* 
         * Found attributes to process.
         * Return with the list of attributes in the html page.
         */

        // Build a form.
        // The name of INPUT field is propertyName[i] '+' propertyType[i]
        // We can be sure about the type, when receiving the form request.
        //
        add2Page("<FORM ACTION=" + HtmlDef.SETFORM + toUrlName(objNameStr) + " METHOD=GET>");

        boolean hasInput = false;

        // Add the table header.
        //
        add2Page("<TABLE ALIGN=center BORDER=1 WIDTH=100% CELLPADDING=3>" );
        add2Page("<TR>");
        add2Page("<TH WIDTH=23%> Name </TH><TH WIDTH=35%> Type </TH><TH WIDTH=7%> Access </TH><TH WIDTH=35%> Value </TH>");
        add2Page("</TR>");

        for (int i = 0; i < propertyName.length; i++) {

            if (propertyName[i] == null ) {
                continue; // next iter of for loop (ie next attribute)
            }

            String attType = usualType(attinfo[i].getType());

            // Start an attribute entry for the table.
            //
            add2Page("<TR>");

            // Add the property cell to the attribute entry.
            //
            String ai = attinfo[i].getDescription();
            if (ai != null && ai.length() >0) {
                add2Page("<TD><A HREF=\"javascript:alert('"+translateNameToHtmlFormat(ai)+"');\"><B>" + propertyName[i] + "</B></A></TD>" );
            } else {
                add2Page("<TD><B>" + propertyName[i] + "</B></TD>" );
            }

            // Add the property type cell to the attribute entry.
            //
            add2Page("<TD ALIGN=RIGHT>" + attType + "</TD>" );

            // Add the access cell and value cell to the attribute entry.
            // NOTE: The choice of representation for the value depends
            // on the property access.
            //
            // READ ONLY
            if (attinfo[i].isReadable() && ! attinfo[i].isWritable()){
                add2Page("<TD ALIGN=center> RO </TD>");

		if (propertyView[i].startsWith("Enumerated")) {			// handle Enumerated case first as Enumerated is not
                    add2Page("<TD>" +propertyView[i]. substring(10) + "</TD>"); // recognized by the checkType method.
		} else if (propertyView[i].startsWith(HtmlDef.HTML_UNAVAILABLE)) {
                    add2Page("<TD>" + propertyView[i] + "</TD>");
                } else if (attType != null && 
			   attType.equals("javax.management.ObjectName")) {
                    add2Page("<TD><A HREF=\""+ HtmlDef.VIEWOBJECTRES+toUrlName(propertyView[i]) +"\">"+ propertyView[i] +"</A></TD>");
                } else if (isArrayType(attType)) {
                    add2Page(buildArrayRef(propertyName[i], objNameStr, "view the values of "+propertyName[i]));
		} else if (!checkType(attType)) {
                    add2Page("<TD><I>Type Not Supported</I>: ["+ translateNameToHtmlFormat(propertyView[i]) +"]</TD>"); // toString() output
                } else { 
                    add2Page("<TD><PRE>" + translateNameToHtmlFormat(propertyView[i]) + "</PRE></TD>");
                }
            }
            // WRITE ONLY
            else if (!attinfo[i].isReadable() && attinfo[i].isWritable()){
                hasInput = true; // flag: we will need to add an "Apply" button

                add2Page("<TD ALIGN=center> WO </TD>");

		if (propertyView[i].startsWith("<SELECT NAME=\"(Enumerated)")) {
		    add2Page("<TD>"+propertyView[i]+"</TD>");
		} else if (!checkType(attType)) {
                    add2Page("<TD><I>Type Not Supported</I></TD>");
                } else if (attType.endsWith("Boolean") || attType.endsWith("boolean")) {
                    add2Page("<TD>"+boolToHtml(propertyName[i],attType,propertyView[i],false)+"</TD>");
                } else {
                    if (isArrayType(attType)) {
                        if (!checkType(attType)) {
                            propertyView[i] = "Write only for "+propertyName[i]+"[] Not supported ";
                        }
                        add2Page("<TD>"+propertyView[i]+"</TD>");
                    } else {
                        add2Page("<TD><INPUT TYPE=\"text\" NAME=\""+ propertyName[i] +"+"+ attType +"\" SIZE=34%></TD>"); 
                    }
                }

            } 
            // READ WRITE
            else if (attinfo[i].isReadable() && attinfo[i].isWritable()){
                hasInput = true; // flag: we will need to add an "Apply" button

                add2Page("<TD ALIGN=center> RW </TD>");

		if (propertyView[i].startsWith("<SELECT NAME=\"(Enumerated)")) {
		    add2Page("<TD>"+propertyView[i]+"</TD>");
		} else if (propertyView[i].startsWith(HtmlDef.HTML_UNAVAILABLE)) {
                    add2Page("<TD>" + propertyView[i] + "</TD>");
		} else if (!checkType(attType)) {
                    add2Page("<TD><I>Type Not Supported</I>: ["+ translateNameToHtmlFormat(propertyView[i]) +"]</TD>"); // toString() output
                } else if (propertyView[i].startsWith("<SELECT NAME=\"(Enumerated)")) {
                    add2Page("<TD>"+propertyView[i]+"</TD>");
                } else if (attType.endsWith("Boolean") || attType.endsWith("boolean")) {
                    add2Page("<TD>" + boolToHtml(propertyName[i],attType,propertyView[i],true)+ "</TD>");
                } else if (isArrayType(attType)) {
                    add2Page(buildArrayRef(propertyName[i], objNameStr, "view the values of "+propertyName[i]));
                } else {
                    htmlPage.append("<TD><INPUT TYPE=\"text\" NAME=\"" +
                                    propertyName[i] +  "+" + attType + "\" ");
                    htmlPage.append("VALUE=\"" + translateNameToHtmlFormat(propertyView[i]) + "\" ");
                    htmlPage.append("SIZE=34%");
                    add2Page(">");
                    if (attType.equals("javax.management.ObjectName")) {
                        if (propertyView[i].equals(" ") || propertyView[i].equals("")) {
                            htmlPage.append("<A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(propertyView[i])+"\"></A>");
                        } else {
                            htmlPage.append("<A HREF=\""+HtmlDef.VIEWOBJECTRES+toUrlName(propertyView[i])+"\">view</A>");
                        }
                    } 
                    add2Page("</TD>"); 
                }
            } else {
                add2Page("<TD> ?? </TD>");
                add2Page("<TD>" + propertyView[i] + "</TD>");
            }
            // End of the attribute entry.
            //
            add2Page("</TR>");

        } // END for LOOP

        // End of the attribute table.
        //
        add2Page("</TABLE>");

        // Add the button to send back the modifications,
        // if there is any input fields.
        //
        add2Page("<P>");
        add2Page("<TABLE WIDTH=100%><TR>");
        if (hasInput) {
            add2Page("<TD ALIGN=LEFT><INPUT TYPE=submit VALUE=\""+HtmlDef.setChangesBut+"\"></TD>");
        } else {
            add2Page("<TD ALIGN=LEFT></TD>");
        }
        add2Page("</TR></TABLE></FORM>");
    }


    /**
     * Builds the operations part of the object(MBean) page.
     */
    private void buildOperations() {
        if (logger.finerOn()) {
            logger.finer("buildOperations","Build operations part");
        }

        String actionStr = null;
        String ai = null;
        MBeanOperationInfo[] op = 
	    checkedOperationInfo(manipulatedObj.getOperations());

        // Build a list of MBean operations.
        // List format:
        //
        // operation description
        //
        // +-------------+-----------+------------+
        // | return type | operation | parameters |
        // |             |           |  ...       |
        // +-------------+-----------+------------+
        // ...
        // +-------------+-----------+------------+
        //
        add2Page("<HR><H3>List of MBean operations:</H3>"+HtmlDef.PF);

        if (logger.finerOn()) {
            logger.finer("buildOperations","Found "+op.length+" operation(s)");
        }

        /* 
         * No operations to process.
         * Return with an appropriate message in html page.
         */
        if (op.length == 0) {
            add2Page("<I>No Operations</I><P>");
            return;
        }

        /* 
         * Found operations to process.
         * Return with the list of operations in the html page.
         */
        for (int i = op.length-1; i >=0 ; i--) {
            ai = op[i].getDescription();

            // Build the operation button and input fields form.
            //
            actionStr = buildOperationWithParam(op[i].getReturnType(), op[i].getName(), op[i].getSignature());

            // Add the operation description and
            // the operation button and input fields form.
            //
            if (actionStr != null) {
                if (ai != null && ai.length() > 0) {
                    add2Page("<HR><A HREF=\"javascript:alert('"+translateNameToHtmlFormat(ai)+"');\"><B>Description of " +op[i].getName() + "</B></A>");
                } else {
                    add2Page("<HR><B>"+op[i].getName()+"</B>");
                }
                add2Page(actionStr);
            }
        }
    }


    /**
     * Builds a form containing an operation with its possible parameters.
     */
    private String buildOperationWithParam(String returnType, String action, MBeanParameterInfo[] paramList) {
        if (logger.finerOn()) {
            logger.finer("buildOperationWithParam","Build operation for = "+action);
        }

        boolean support = true;
        String propType = null;
        String param = null;
        StringBuffer str = new StringBuffer(50);

        // Do we support the operation ?
        //
        int max = paramList.length;
        for (int i = 0; i < max; i++) {
            propType = paramList[i].getType();
            if (propType == null || 
		!checkType(propType) || 
		isArrayType(propType)) {
                support = false;
                break;
            }
        }
        if (!support) {
            str.append("  <I>(Operation Not Supported)</I><P>");
        }

        // Start a form, containing the operation table.
        // 
        if (support) {
            str.append("<FORM ACTION=" + HtmlDef.INVOKEACTION + toUrlName(manipulatedObjName.toString())+"/action="+action);
            str.append( " METHOD=GET>" + HtmlDef.PF);
        }
        str.append("<TABLE>" + HtmlDef.PF);

        // Add the operation return type entry to the operation table.
        //
        str.append("<TR><TD>"+returnType+"</TD>"+HtmlDef.PF);

        // Add the operation name to the operation table.
        //
        if (!support) {
            str.append("<TD>"+action+"</TD>"+HtmlDef.PF);
        } else {
            str.append("<TD><INPUT TYPE=SUBMIT NAME=\"action\" VALUE=\""+action+"\"></TD>"+HtmlDef.PF);
        }

        // Analyse the parameters and add them to operation table.
        //
        for (int i = 0; i < max; i++) {
            propType = paramList[i].getType();

            // Build the paramaters.
            //
            if (paramList[i].getName() != null &&
		paramList[i].getName().length() > 0) {
                param = paramList[i].getName();
            } else {
                param = "param"+i;
            }
            if (i != 0) str.append("<TD></TD>");
            String ai = paramList[i].getDescription();
            if (ai != null && ai.length() > 0) {
                str.append("<TD>("+propType+")<A HREF=\"javascript:alert('"+translateNameToHtmlFormat(ai)+"');\">" +param+"</A></TD>"+HtmlDef.PF);
            } else {
                str.append("<TD>("+propType+")" +param+"</TD>"+HtmlDef.PF);
            }

            if (!support) {
                str.append("<TD></TD>" + HtmlDef.PF);
            } else if (propType.endsWith("Boolean") || propType.endsWith("boolean")) {
                str.append("<TD>" + boolToHtml(param, propType,"true",true) + "</TD>" + HtmlDef.PF);
            } else {
                // Add input to the form.
                //
                str.append("<TD><INPUT TYPE=\"text\" NAME=\"" + param +  "+" + propType + "\" ");
                str.append("SIZE=50%");
                str.append( "></TD>" + HtmlDef.PF);    
            }
            str.append("</TR><TR><TD></TD>" + HtmlDef.PF);
        }

        // End the form and operation table.
        //
        str.append("</TR></TABLE>" + HtmlDef.PF);
        if (support) {
            str.append("</FORM>" + HtmlDef.PF);
        }

        return str.toString();
    }


    /**
     * Builds the link to the array page.
     */    
    private String buildArrayRef(String name, String objName, String value) {
        return new String("<TD> <A HREF=\"" + HtmlDef.VIEWPROPERTY + "/" +
                          name + toUrlName(objName) + "\">" + value + "</A> </TD>");
    }


    private String getStringValue(ObjectName o, MBeanAttributeInfo attrInfo) {

        Object result = null;
	boolean r = attrInfo.isReadable();
	boolean w = attrInfo.isWritable();

        // Get the attribute value.
        //
        try {
            if (logger.finerOn()) {
                logger.finer("getStringValue","Build displayable value for attribute ["+ attrInfo.getName() +"] of object ["+o.toString()+"]");
            }
	    if (r == true) {
            result = mbs.getAttribute(o, attrInfo.getName()) ;
	    }
        } catch (Exception e) {
	    Exception ee = null ;
	    if (e instanceof javax.management.RuntimeMBeanException) {
		ee = ((javax.management.RuntimeMBeanException) e).getTargetException();
	    } else if (e instanceof javax.management.MBeanException) {
		ee = ((javax.management.MBeanException) e).getTargetException();
	    }
	    if (logger.finestOn()) {
		logger.finest("getStringValue","Cannot get value: "+ e +" [wrapped exception: "+ ee +"]");
	    }
	    if (ee == null) {
		return (HtmlDef.HTML_UNAVAILABLE +": ["+ e.toString() +"]");
	    } else {
		return (HtmlDef.HTML_UNAVAILABLE +": "+ e.getClass().getName() +" wraps ["+ ee.toString() +"]");
	    }
        }
	
        // Date to representable HTML format.
        //
        if (result instanceof Date){
            DateFormat df = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);
            // java BUG : the TZ of the DateFormat is not set correctly with the method getDateTimeInstance.
            // By default, the DateFormat use the TZ of the system.
            //
            df.setTimeZone(TimeZone.getDefault());
            return df.format( (Date) result );
        }

        // com.sun.jdmk.Enumerated to representable HTML format.
        //
	try {
	    Class c ;
	    if (result != null) {
		c = result.getClass();
	    } else {
		c = loadClass(attrInfo.getType());
	    }
	    Class sc   = c.getSuperclass();
	    // [JF] begin debug
//             if (logger.finestOn()) {
// 		boolean t1 = (sc != null);
// 		boolean t2 = (Class.forName("com.sun.jdmk.Enumerated").isAssignableFrom(sc));
//                 logger.finest("getStringValue","Attribute ["+ attrInfo.getName() +"] is of type["+ c +"] and inherits from ["+ sc +
// 		      "]. Enum tests return ["+ t1 +", "+ t2 +"]");
//             }
	    // [JF] end debug
	    if ( (sc != null) && (com.sun.jdmk.Enumerated.class.isAssignableFrom(sc)) ) {
                return enumToHtml((Enumerated)result, attrInfo) ;
	    }
	} catch (ClassNotFoundException e) {
            if (logger.finestOn()) {
                logger.finest("getStringValue","Cannot find class "+ attrInfo.getType() +":"+ e);
            }
	}

        // If the value is null (case for WO attr), returns empty string.
        //
        if (result == null) {
            return ("");
        }

        return result.toString() ; // so, even for unsupported types we return the output of toString() 
    }

    private Object stringToObject(String typeStr, String valueStr){
        Object value = null;
        try {
            if (typeStr.endsWith("String")) {
                value = valueStr;
            }
            else if (typeStr.endsWith("Byte") || typeStr.endsWith("byte")) {
                value = new Byte(valueStr);
            }
            else if (typeStr.endsWith("Long") || typeStr.endsWith("long")) {
                value = new Long(valueStr);
            }
            else if (typeStr.endsWith("Integer") || typeStr.endsWith("int")) {
                value = new Integer(valueStr);
            }
            else if (typeStr.endsWith("Date")) {
                try {
                    DateFormat df = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG);
                    // BUG : the TZ of the DateFormat is not set correctly with the method getDateTimeInstance.
                    // By default, the DateFormat use the TZ of the system.
                    df.setTimeZone(TimeZone.getDefault());
                    value = df.parse(valueStr);
                } catch (java.text.ParseException e){
                    buildError("Cannot convert String \""+ valueStr +"\" to " +
                               typeStr + 
                               ".<P>",HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                    return null;
                }
            }
            else if (typeStr.endsWith("Boolean") || typeStr.endsWith("boolean")) {
                value = new Boolean(valueStr);
            }
            else if (typeStr.endsWith("Number")) {
                try {
                    value = new Integer(valueStr) ;
                }
                catch(NumberFormatException e1) {
                    try {
                        value = new Long(valueStr) ;
                    }
                    catch(NumberFormatException e2) {
                        try {
                            value = new Float(valueStr) ;
                        }
                        catch(NumberFormatException e3) {
                            try {
                                value = new Double(valueStr) ;
                            }
                            catch(NumberFormatException e4){
                                buildError("Cannot convert String \""+ valueStr +"\" to " + typeStr +".<P>",
                                           HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
                                return null;
                            }
                        }
                    }
                }
            }
            else if (typeStr.equals("javax.management.ObjectName")) {
                try {
                    value = new ObjectName(valueStr);
                } catch (MalformedObjectNameException e) {
                    buildError("Cannot convert String \""+ valueStr +"\" to " + typeStr + ".<P>",
                               HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME_ID + " " + HtmlDef.HTTP_ERROR_MALFORMED_OBJECTNAME);
                    return null;
                }
            } 
            else if (typeStr.endsWith("Character") || typeStr.endsWith("char") ){
                value = new Character(valueStr.charAt(0));
            }
            else if (typeStr.endsWith("Double") || typeStr.endsWith("double")) {
                value = new Double(valueStr);
            }
            else if (typeStr.endsWith("Float") || typeStr.endsWith("float")) {
                value = new Float(valueStr);
            }
            else if (typeStr.endsWith("Short") || typeStr.endsWith("short")) {
                value = new Short(valueStr);
            }
            else {
                // Unknown conversion mechanism for this.
                //
                buildError("Cannot convert the String \""+ valueStr +"\" to " + typeStr,
                           HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE_ID + " " + HtmlDef.HTTP_ERROR_INVALID_PROP_VALUE);
                return null;
            }
        } catch (NumberFormatException e) {
            buildError("Cannot convert the String \""+ valueStr +"\" to " + typeStr + ".<P>",
                       HtmlDef.HTTP_ERROR_BAD_REQUEST_ID + " " + HtmlDef.HTTP_ERROR_BAD_REQUEST);
            return null;
        }
        return value;
    }
    
    private Object getArray(String typeStr, ArrayList a){
        Object value = null;
        int l = a.size();
        int i;
        if (typeStr.endsWith("String")) {
            value = a.toArray(new String[l]);
        }
        else if (typeStr.endsWith("Byte") ){
            value = a.toArray(new Byte[l]);
        }
        else if (typeStr.endsWith("byte")) {
            value = new byte[l];
            for (i=l-1;i>=0;i--)
                ((byte[])value)[i]=((Byte)a.get(i)).byteValue();
        }
        else if (typeStr.endsWith("Long")){
            value = a.toArray(new Long[l]);
        } 
        else if (typeStr.endsWith("long")) {
            value = new long[l];
            for (i=l-1;i>=0;i--)
                ((long[])value)[i]=((Long)a.get(i)).longValue();
        }
        else if (typeStr.endsWith("Integer")) {
            value = a.toArray(new Integer[l]);
        } 
        else if (typeStr.endsWith("int")) {
            value = new int[l];
            for (i=l-1;i>=0;i--)
                ((int[])value)[i]=((Integer)a.get(i)).intValue();
        }
        else if (typeStr.endsWith("Date")) {
            value = a.toArray(new Date[l]);
        }
        else if (typeStr.endsWith("Boolean")) {
            value = a.toArray(new Boolean[l]);
        }
        else if (typeStr.endsWith("boolean")) {
            value = new boolean[l];            
            for (i=l-1;i>=0;i--)
                ((boolean[])value)[i]=((Boolean)a.get(i)).booleanValue();
        }
        else if (typeStr.equals("javax.management.ObjectName")) {
            value = a.toArray(new ObjectName[l]);
        } 
        else if (typeStr.equals("Character")){
            value = a.toArray(new Character[l]);
        }
        else if (typeStr.equals("char")) {
            value = new char[l];
            for (i=l-1;i>=0;i--)
                ((char[])value)[i]=((Character)a.get(i)).charValue();
        } 
        else if (typeStr.equals("Double")) {
            value = a.toArray(new Double[l]);
        } 
        else if (typeStr.equals("double")) {
            value = new double[l];
            for (i=l-1;i>=0;i--)
                ((double[])value)[i]=((Double)a.get(i)).doubleValue();
        } 
        else if (typeStr.equals("Float")) {
            value = a.toArray(new Float[l]);
        } 
        else if (typeStr.equals("float")) {
            value = new float[l];
            for (i=l-1;i>=0;i--)
                ((float[])value)[i]=((Float)a.get(i)).floatValue();
        }          
        else {
            // Unknown conversion mechanism for this
            //
            return null;
        }
        return value;
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
    
    private MBeanAttributeInfo[] attinfo = null;
    private String propertyName[] = null;
    private String propertyView[] = null;
    private ObjectName manipulatedObjName = null;
    private MBeanInfo manipulatedObj = null;    
    private String meta = null;
    private int autoRefresh = 0;
    private final int minAutoRefresh = 5;
    private HtmlAdaptorServer server = null;
}
