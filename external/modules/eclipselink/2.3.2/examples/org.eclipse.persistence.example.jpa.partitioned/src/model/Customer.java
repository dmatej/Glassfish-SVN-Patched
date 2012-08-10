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
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import org.eclipse.persistence.annotations.HashPartitioning;
import org.eclipse.persistence.annotations.Partitioned;
import org.eclipse.persistence.annotations.ReplicationPartitioning;

/**
 * Customer, hash partitioned by Id.
 * @author James Sutherland
 */
@Entity
@Table(name="PART_CUSTOMER")
@ReplicationPartitioning(name="Replicate", connectionPools={"default","node2"})
@HashPartitioning(name="HashPartitionById", partitionColumn=@Column(name="ID"), connectionPools={"default","node2"})
@Partitioned("HashPartitionById")
public class Customer implements Serializable {
    @Id
    @GeneratedValue(strategy=GenerationType.TABLE)
    private long id;
    @Basic
    private String name;
    @OneToMany(mappedBy="customer")
    private List<Order> orders = new ArrayList<Order>();

    public Customer() {
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Order> getOrders() {
        return orders;
    }

    public void setOrders(List<Order> orders) {
        this.orders = orders;
    }
    
    public void addOrder(Order order) {
        order.setCustomer(this);
        getOrders().add(order);
    }
}
