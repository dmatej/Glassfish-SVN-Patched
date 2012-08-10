/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 *              mbraeuer - annotated version
 ******************************************************************************/
package eclipselink.example.jpa.employee.model;

import java.sql.Date;

import javax.persistence.Embeddable;

/**
 * Represents the period of time an employee has worked for the company. A null
 * endDate indicates that the employee is current.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Embeddable
public class EmploymentPeriod {

    private Date startDate;

    private Date endDate;

    public Date getStartDate() {
        return this.startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return this.endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }
}
