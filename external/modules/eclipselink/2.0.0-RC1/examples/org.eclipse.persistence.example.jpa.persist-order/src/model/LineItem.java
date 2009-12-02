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

import javax.persistence.*;

@Entity
@IdClass(LineItem.ID.class)
@Table(name = "PO_LINE_ITEM")
public class LineItem implements Serializable {
	@Id
	@Column(name = "ORDER_NUM", insertable = false, updatable = false)
	private String orderNumber;

	@ManyToOne
	@JoinColumn(name = "ORDER_NUM")
	private Order order;

	@Column(name = "LINE_NUM")
	private int lineNumber = -1;

	@Id
	@Column(name = "PROD_ID", insertable = false, updatable = false)
	private int productId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "PROD_ID")
	private Product product;

	private int quantity;

	private double price;

	public LineItem() {
	}

	public LineItem(Product product, int quantity, double price) {
		this();
		setProduct(product);
		setQuantity(quantity);
		setPrice(price);
	}

	public String getOrderNumber() {
		return this.orderNumber;
	}

	public Order getOrder() {
		return this.order;
	}

	public void setOrder(Order order) {
		this.order = order;
		this.orderNumber = order.getOrderNumber();
	}

	public int getProductId() {
		return this.productId;
	}

	public int getQuantity() {
		return this.quantity;
	}

	public void setQuantity(int quantity) {
		this.quantity = quantity;
	}

	public double getPrice() {
		return this.price;
	}

	public void setPrice(double price) {
		this.price = price;
	}

	public int getLineNumber() {
		return this.lineNumber;
	}

	public Product getProduct() {
		return this.product;
	}

	public void setProduct(Product product) {
		this.product = product;
		this.productId = product.getId();
	}

	public String toString() {
		return "LineItem(orderNum=" + getOrderNumber() + ", prodId=" + getProductId() + ", " + "lineNum=" + getLineNumber() + ")";
	}

	public static class ID implements Serializable {
		public String orderNumber;
		public int productId;

		public ID() {
		}

		public ID(String orderNumber, int productId) {
			this.orderNumber = orderNumber;
			this.productId = productId;
		}

		public boolean equals(Object other) {
			if (other instanceof ID) {
				final ID otherID = (ID) other;
				return otherID.orderNumber == orderNumber && otherID.productId == productId;
			}
			return false;
		}

		public int hashCode() {
			return this.orderNumber.hashCode() + this.productId;
		}
	}

}
