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

import javax.persistence.CascadeType;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToOne;

import org.eclipse.persistence.annotations.DiscriminatorClass;
import org.eclipse.persistence.annotations.VariableOneToOne;

/**
 * Represents an insurance claim.
 * Root of the Claim hierarchy.
 */
@Entity
@Inheritance(strategy=InheritanceType.SINGLE_TABLE)
public class Claim extends PersistenceObject {
    private String description;
    
    private BigDecimal amount;

    @ManyToOne(fetch=FetchType.LAZY, cascade=CascadeType.PERSIST)
    private Policy policy;
    
    // TODO: Setting targetInterface should not be required.
    @VariableOneToOne(targetInterface=PolicyHolder.class,
            fetch=FetchType.LAZY,
            discriminatorColumn=@DiscriminatorColumn(name="CLAIMANT_TYPE"),
            discriminatorClasses={@DiscriminatorClass(discriminator="CORP", value=CorporateHolder.class),@DiscriminatorClass(discriminator="PER", value=PersonalHolder.class)})
    private PolicyHolder claimant;

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(long amount) {
        setAmount(new BigDecimal(amount));
    }

    public void setAmount(double amount) {
        setAmount(new BigDecimal(String.valueOf(amount)));
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public Policy getPolicy() {
        return policy;
    }

    public void setPolicy(Policy policy) {
        this.policy = policy;
    }

    public PolicyHolder getClaimant() {
        return claimant;
    }

    public void setClaimant(PolicyHolder claimant) {
        this.claimant = claimant;
    }

    public String toString() {
        return getClass().getSimpleName() + ":" + getDescription();
    }
}
