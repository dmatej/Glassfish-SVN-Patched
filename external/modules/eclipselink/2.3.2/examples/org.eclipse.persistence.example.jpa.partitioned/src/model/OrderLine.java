/*******************************************************************************
 * Copyright (c) 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      Oracle - initial impl
 ******************************************************************************/
package model;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.*;

import org.eclipse.persistence.annotations.Partitioned;

/**
 * OrderLine, help by Order, hash partitioned by the Order's Id.
 * @author James Sutherland
 */
@Entity
@Table(name="PART_ORDERLINE")
@Partitioned("HashPartitionByOrderId")
public class OrderLine implements Serializable {
    @Id
    @ManyToOne(fetch=FetchType.LAZY)
    @JoinColumn(name="ORDER_ID")
    private Order order;
    @Id
    private int lineNumber;
    @Basic
    private String description;
    @Basic
    private BigDecimal cost = BigDecimal.valueOf(0);

    public OrderLine() {
    }
    
    public OrderLine(String description, BigDecimal cost) {
        this.description = description;
        this.cost = cost;
    }

    public Order getOrder() {
        return order;
    }

    public void setOrder(Order order) {
        this.order = order;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigDecimal getCost() {
        return cost;
    }

    public void setCost(BigDecimal cost) {
        this.cost = cost;
    }
    
}
