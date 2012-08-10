/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              James Sutherland - initial example
 ******************************************************************************/
package model;

import java.util.List;

/**
 * Public interface of the PolicyHolderImpl.
 */
public interface PolicyHolder  {
    long getId();
    
    String getName();
    
    void setName(String name);
    
    List<Policy> getPolicies();
    
    List<Claim> getClaims();
    
    void addClaim(Claim claim);
    
    void addPolicy(Policy policy);
}
