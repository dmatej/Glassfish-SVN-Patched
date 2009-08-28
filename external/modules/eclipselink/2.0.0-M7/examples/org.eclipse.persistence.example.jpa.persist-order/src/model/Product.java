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
@Table(name="PO_PRODUCT")
public class Product implements Serializable {
	@Id
	private int id;
	
	private String name;
	
	@Column(name="DESCRIP")
	private String description;
	
	private double price;

	public int getId() {
		return this.id;
	}

	public Product() {
		
	}
	
	public Product(int id, String name, String description, double price) {
		this();
		this.id = id; 
		setName(name);
		setDescription(description);
		setPrice(price);
	}
	
	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public double getPrice() {
		return this.price;
	}

	public void setPrice(double price) {
		this.price = price;
	}
}
