/*
 * @(#)file      Repository.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.27
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


// java import
import java.util.ArrayList;
import java.util.Set;


// RI import
import javax.management.*;

/**
 * The Repository interface provides local access to the
 * implementation of Repository Service in use in the agent.
 *
 * @since Java DMK 5.0
 * @deprecated This interface is kept as a utility class, though it is 
 * no longer used. It may not be supported in future releases.
 */
public interface Repository   { 


    /**
     * The purpose of this method is to provide a unified way to provide whatever
     * configuration information is needed by the specific underlying implementation
     * of the repository.
     *
     * @param configParameters An list containing the configuration parameters needed by the specific
     * Repository Service implementation.
     */
    public void setConfigParameters(ArrayList configParameters) ; 

    /**
     * Indicates whether or not the Repository Service supports filtering. If
     * the Repository Service does not support filtering, the MBean Server
     * will perform filtering.
     *
     * @return  true if filtering is supported, false otherwise.
     */
    public boolean isFiltering() ; 
    
    /**
     * Stores an MBean associated with its object name in the repository.
     *
     *@param object MBean to be stored in the repository.
     *@param name MBean object name.
     *     
     *@exception InstanceAlreadyExistsException  The MBean is already stored in the repository.
     */        
    public void addMBean(Object object, ObjectName name )
	throws InstanceAlreadyExistsException ;
    
    /**
     * Checks whether an MBean of the name specified is already stored in
     * the repository.
     *
     * @param name name of the MBean to find.
     *
     * @return  true if the MBean is stored in the repository, false otherwise.
     */
    public boolean contains(ObjectName name) ; 
    
    /**
     * Retrieves the MBean of the name specified from the repository. The
     * object name must match exactly.
     *
     * @param name name of the MBean to retrieve.
     *
     * @return  The retrieved MBean if it is contained in the repository, null otherwise.
     *
     */
    public Object retrieve(ObjectName name) ; 

    /**
     * Selects and retrieves the list of MBeans whose names match the specified
     * object name pattern and which match the specified query expression (optionally).
     *
     *
     * @param name The name of the MBean(s) to retrieve - may be a specific object or
     * a name pattern allowing multiple MBeans to be selected.     
     * @param query query expression to apply when selecting objects - this parameter will
     * be ignored when the Repository Service does not support filtering.
     *
     * @return  The list of MBeans selected. There may be zero, one or many MBeans returned
     * in the Set.
     *
     */
    public Set query(ObjectName name, QueryExp query);
       
    /**
     * Removes an MBean from the repository.
     *
     * @param name name of the MBean to remove.
     *
     * @exception InstanceNotFoundException The MBean does not exist in the repository.
     */
    public void remove(ObjectName name) throws InstanceNotFoundException ; 

    /**
     * Gets the number of MBeans stored in the repository.
     *
     * @return  Number of MBeans.
     */
    public Integer getCount() ; 

    /**
     * Gets the name of the domain currently used by default in the repository.
     *
     * @return  A string giving the name of the default domain name.
     */
    public String getDefaultDomain() ; 

    /**
     * Sets the name of the domain currently used by default in the repository.
     *
     * @param domain the default domain name.
     */
 public void setDefaultDomain(String domain) ; 

 }
