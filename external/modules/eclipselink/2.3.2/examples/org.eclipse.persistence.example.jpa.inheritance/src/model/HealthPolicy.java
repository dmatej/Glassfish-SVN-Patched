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

import javax.persistence.Entity;

/**
 * Represents a health insurance policy.
 * Stored in both POLICY and HEALTHPOLICY tables.
 */
@Entity
public class HealthPolicy extends Policy {
    private double dentalRate = 0.5;
    private double drugRate = 0.5;
    private double surgeryRate = 0.5;
    private double coverageRate = 0.5;
    
    public double getDentalRate() {
        return dentalRate;
    }
    public void setDentalRate(double dentalRate) {
        this.dentalRate = dentalRate;
    }
    public double getDrugRate() {
        return drugRate;
    }
    public void setDrugRate(double drugRate) {
        this.drugRate = drugRate;
    }
    public double getSurgeryRate() {
        return surgeryRate;
    }
    public void setSurgeryRate(double surgeryRate) {
        this.surgeryRate = surgeryRate;
    }
    public double getCoverageRate() {
        return coverageRate;
    }
    public void setCoverageRate(double coverageRate) {
        this.coverageRate = coverageRate;
    }    
}
