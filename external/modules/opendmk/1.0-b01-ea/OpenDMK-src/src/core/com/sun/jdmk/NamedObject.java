/*
 * @(#)file      NamedObject.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.23
 * @(#)date      07/04/04
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
 */

package com.sun.jdmk;

import javax.management.* ; 
 


/**
 * This class is used for storing a pair (name, object) where name is
 * an object name and object is a reference to the object.
 *
 * @since Java DMK 5.0
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases.
 */
public class NamedObject  { 


    /**
     * Object name.
     */
    private ObjectName name;
    
    /**
     * Object reference.
     */
    private Object object= null;
    

    /**
     * Allows a named object to be created.
     *
     *@param objectName The object name of the object.     
     *@param object A reference to the object.
     */
    public NamedObject(ObjectName objectName, Object object)  { 
	if (objectName.isPattern()) {
	    throw new RuntimeOperationsException(new IllegalArgumentException("Invalid name->"+ objectName.toString()));
	}	
	this.name= objectName;
	this.object= object;
    } 

    /**
     * Allows a named object to be created.
     *
     *@param objectName The string representation of the object name of the object.
     *@param object A reference to the object.
     *
     *@exception MalformedObjectNameException The string passed does not have the format of a valid ObjectName
     */
    public NamedObject(String objectName, Object object) throws MalformedObjectNameException{ 
	ObjectName objName= new ObjectName(objectName);
	if (objName.isPattern()) {
	    throw new RuntimeOperationsException(new IllegalArgumentException("Invalid name->"+ objName.toString()));
	}	
	this.name= objName;
	this.object= object;	
    } 
  
    /**
     * Compares the current object name with another object name.
     *
     * @param object  The Named Object that the current object name is to be 
     *        compared with.
     *
     * @return  True if the two named objects are equal, otherwise false.
     */
    public boolean equals(Object object)  {
        if (this == object) return true;
        if (object == null) return false;
        if (!(object instanceof NamedObject)) return false;
        NamedObject no = (NamedObject) object;
        return name.equals(no.getName());
    }


    /**
     * Returns a hash code for this named object.
     *
     */   
    public int hashCode() {
        return name.hashCode();
    }

    /**
     * Get the object name.
     */
    public ObjectName getName()  { 
	return name;
    } 
   
    /**
     * Get the object
     */
    public Object getObject()  { 
	return object;
   } 
    
 }
