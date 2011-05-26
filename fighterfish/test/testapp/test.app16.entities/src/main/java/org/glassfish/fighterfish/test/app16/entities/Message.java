package org.glassfish.fighterfish.test.app16.entities;

import java.io.Serializable;
import java.lang.Long;
import java.lang.String;
import javax.persistence.*;

/**
 * Entity implementation class for Entity: Message
 *
 */
@Entity
public class Message implements Serializable {

	   
	@Id @GeneratedValue
	private Long id;
	private String value;
	private static final long serialVersionUID = 1L;

	public Message() {
		super();
	}   
	public Long getId() {
		return this.id;
	}

	public void setId(Long id) {
		this.id = id;
	}   
	public String getValue() {
		return this.value;
	}

	public void setValue(String value) {
		this.value = value;
	}
   
}
