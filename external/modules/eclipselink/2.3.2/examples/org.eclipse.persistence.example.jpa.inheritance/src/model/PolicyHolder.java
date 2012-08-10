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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToMany;

/**
 * Represents an insurance policy holder.
 */
@Entity
public class PolicyHolder extends PersistenceObject {
    private String name;

    @OneToMany(mappedBy="holder", cascade=CascadeType.ALL)
    private List<Policy> policies = new ArrayList<Policy>();

    @OneToMany(mappedBy="claimant", cascade=CascadeType.PERSIST)
    private List<Claim> claims = new ArrayList<Claim>();
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Policy> getPolicies() {
        return policies;
    }

    public void addPolicy(Policy policy) {
        getPolicies().add(policy);
        policy.setHolder(this);
    }

    public void setPolicies(List<Policy> policies) {
        this.policies = policies;
    }

    public List<Claim> getClaims() {
        return claims;
    }

    public void addClaim(Claim claim) {
        getClaims().add(claim);
        claim.setClaimant(this);
    }

    public void setClaims(List<Claim> claims) {
        this.claims = claims;
    }
    
    public String toString() {
        return getClass().getSimpleName() + ":" + getName();
    }
}
