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
 *      Oracle - initial impl
 ******************************************************************************/
package model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import org.eclipse.persistence.annotations.HashPartitioning;
import org.eclipse.persistence.annotations.Partitioned;

/**
 * Order, hash partitioned by the Order's Id.
 * @author James Sutherland
 */
@Entity
@Table(name="PART_ORDER")
@HashPartitioning(name="HashPartitionByOrderId", partitionColumn=@Column(name="ORDER_ID"), connectionPools={"default","node2"})
@Partitioned("HashPartitionByOrderId")
public class Order implements Serializable {
    @Id
    @GeneratedValue(strategy=GenerationType.TABLE)
    @Column(name="ORDER_ID")
    private long id;
    @Basic
    private String description;
    @Basic
    private BigDecimal totalCost = BigDecimal.valueOf(0);
    @OneToMany(mappedBy="order", cascade=CascadeType.ALL, orphanRemoval=true)
    @OrderBy("lineNumber")
    private List<OrderLine> orderLines = new ArrayList<OrderLine>();
    @ManyToOne(fetch=FetchType.LAZY)
    private Customer customer;

    public Order() {
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigDecimal getTotalCost() {
        return totalCost;
    }

    public void setTotalCost(BigDecimal totalCost) {
        this.totalCost = totalCost;
    }

    public List<OrderLine> getOrderLines() {
        return orderLines;
    }

    public void setOrderLines(List<OrderLine> orderLines) {
        this.orderLines = orderLines;
    }

    public Customer getCustomer() {
        return customer;
    }

    public void setCustomer(Customer customer) {
        this.customer = customer;
    }
    
    /**
     * Add the order line to the order, and set the back reference and update the order cost.
     */
    public void addOrderLine(OrderLine orderLine) {
        orderLine.setOrder(this);
        getOrderLines().add(orderLine);
        orderLine.setLineNumber(getOrderLines().size());
        setTotalCost(getTotalCost().add(orderLine.getCost()));
    }
}
