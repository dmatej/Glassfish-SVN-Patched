package org.eclipse.persistence.example.jpa.comics.model.annotated;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.GeneratedValue;
import javax.persistence.ManyToOne;

@Entity
public class Issue implements Serializable {
	private static final long serialVersionUID = 8916573949567829954L;

	@Id
    @GeneratedValue
	private int id;
	private String comments;
	private String condition;
	private int copies;
	private int issueNum;
	private double pricePaid;
	@ManyToOne
	private Title title;
	private BigDecimal value;	

	public Issue() {
		super();
	}

	public Issue(int id, Title title, int issueNum, double pricePaid,
			BigDecimal value) {
		super();
		this.id = id;
		this.issueNum = issueNum;
		this.pricePaid = pricePaid;
		this.title = title;
		this.value = value;
	}

	public String getComments() {
		return this.comments;
	}
	
	public String getCondition() {
		return this.condition;
	}
	
	public Integer getCopies() {
		return this.copies;
	}
	
	public int getId() {
		return this.id;
	}
	
	public Integer getIssueNum() {
		return this.issueNum;
	}
	
	public double getPricePaid() {
		return this.pricePaid;
	}
	
	protected Title getTitle() {
		return this.title;
	}
	
	public BigDecimal getValue() {
		return this.value;
	}
	
	public void setComments(String comments) {
		this.comments = comments;
	}
	
	public void setCondition(String condition) {
		this.condition = condition;
	}
	
	public void setCopies(Integer copies) {
		this.copies = copies;
	}
	
	public void setId(int id) {
		this.id = id;
	}
	
	public void setIssueNum(Integer issueNum) {
		this.issueNum = issueNum;
	}
	
	public void setPricePaid(double pricePaid) {
		this.pricePaid = pricePaid;
	}
	
	public void setTitle(Title title) {
		this.title = title;
	}
	
	public void setValue(BigDecimal value) {
		this.value = value;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return 
			getTitle().getName() + "\t" +
			getIssueNum();
		
	}

}
