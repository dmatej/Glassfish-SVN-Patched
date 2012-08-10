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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import org.eclipse.persistence.annotations.Customizer;

/**
 * Represents an insurance policy.
 * Root of the Policy hierarchy.
 */
@Entity
@Inheritance(strategy=InheritanceType.JOINED)
@Customizer(PolicyCustomizer.class)
public abstract class Policy extends PersistenceObject {
    private String description;
    
    private BigDecimal coverage;
    
    @ManyToOne(targetEntity=PolicyHolderImpl.class, fetch=FetchType.LAZY)
    private PolicyHolder holder;

    @OneToMany(mappedBy="policy", cascade=CascadeType.ALL)
    private List<Claim> claims = new ArrayList<Claim>();

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigDecimal getCoverage() {
        return coverage;
    }

    public void setCoverage(long coverage) {
        setCoverage(new BigDecimal(coverage));
    }

    public void setCoverage(double coverage) {
        setCoverage(new BigDecimal(String.valueOf(coverage)));
    }

    public void setCoverage(BigDecimal coverage) {
        this.coverage = coverage;
    }

    public PolicyHolder getHolder() {
        return holder;
    }

    public void setHolder(PolicyHolder holder) {
        this.holder = holder;
    }

    public List<Claim> getClaims() {
        return claims;
    }
    
    public void addClaim(Claim claim) {
        getClaims().add(claim);
        claim.setPolicy(this);
    }

    public void setClaims(List<Claim> claims) {
        this.claims = claims;
    }

    public String toString() {
        return getClass().getSimpleName() + ":" + getDescription();
    }
}
