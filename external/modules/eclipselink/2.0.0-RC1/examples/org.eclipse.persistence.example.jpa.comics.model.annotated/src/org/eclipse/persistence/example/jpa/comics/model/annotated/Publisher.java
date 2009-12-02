package org.eclipse.persistence.example.jpa.comics.model.annotated;

import java.io.Serializable;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.GeneratedValue;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;

@Entity
public class Publisher implements Serializable {
    private static final long serialVersionUID = -562987599581461681L;

    @Id
    @GeneratedValue
 	private int id;
	private String name;
	@OneToMany(mappedBy="publisher")
	@OrderBy("name ASC")
	private List<Title> titles;

    public Publisher() {
    }

    public Publisher(int id, String name) {
        this.id = id;
        this.name = name;
    }

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return Returns the id.
     */
    public int getId() {
        return id;
    }
    
	public void setId(int id) {
		this.id = id;
	}

	public String toString() {
        return "Publisher(" + getId() + ", " + getName() + ")";
    }

	public List<Title> getTitles() {
		return titles;
	}

	public void setTitles(List<Title> titles) {
		this.titles = titles;
	}

	public void addTitle(Title aTitle) {
		getTitles().add(aTitle);
		aTitle.setPublisher(this);
	}


}
