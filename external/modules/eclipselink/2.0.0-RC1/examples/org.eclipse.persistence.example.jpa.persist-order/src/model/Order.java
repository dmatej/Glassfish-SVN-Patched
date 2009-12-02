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
 *     dclarke - Example: Maintaining Collection Order (Bug 218321)
 *     			 http://wiki.eclipse.org/EclipseLink/Examples/JPA/Collectionordering
 *     
 *******************************************************************************/
package model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import org.eclipse.persistence.annotations.PrivateOwned;

/**
 * 
 * @author djclarke
 * @since EclipseLink 1.1
 */
@Entity
@Table(name = "PO_ORDER")
public class Order implements Serializable {
	@Id
	@Column(name = "ORDER_NUM")
	private String orderNum;

@OneToMany(mappedBy = "order", cascade = CascadeType.ALL)
@OrderBy("lineNumber")
@PrivateOwned
private List<LineItem> lineItems = new ArrayList<LineItem>();

	@Column(name = "DESCRIP")
	private String description;

	public Order() {

	}

	public Order(String number) {
		this();
		setOrderNumber(number);
	}

	public String getOrderNumber() {
		return this.orderNum;
	}

	public void setOrderNumber(String number) {
		this.orderNum = number;
	}

	public List<LineItem> getLineItems() {
		return this.lineItems;
	}

	public void setLineItems(List<LineItem> lineItems) {
		this.lineItems = lineItems;
	}

	public LineItem addLineItem(LineItem item) {
		getLineItems().add(item);
		item.setOrder(this);
		return item;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public LineItem addLineItem(int i, LineItem item) {
		getLineItems().add(i, item);
		item.setOrder(this);
		return item;
	}
	
}
