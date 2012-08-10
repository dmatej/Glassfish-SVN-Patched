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
 * Stored in both POLICY and VEHICLEPOLICY tables.
 */
@Entity
public abstract class VehiclePolicy extends Policy {
    private int drivers = 1;
    private boolean collision = false;
    private String vehicleMake;
    private String vehicleModel;
    public int getDrivers() {
        return drivers;
    }
    public void setDrivers(int drivers) {
        this.drivers = drivers;
    }
    public boolean isCollision() {
        return collision;
    }
    public void setCollision(boolean collision) {
        this.collision = collision;
    }
    public String getVehicleMake() {
        return vehicleMake;
    }
    public void setVehicleMake(String vehicleMake) {
        this.vehicleMake = vehicleMake;
    }
    public String getVehicleModel() {
        return vehicleModel;
    }
    public void setVehicleModel(String vehicleModel) {
        this.vehicleModel = vehicleModel;
    }
    
}
