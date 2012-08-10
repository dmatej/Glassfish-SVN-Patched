package org.eclipse.persistence.example.jpa.comics.model.annotated;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.GeneratedValue;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;

@Entity
public class Title implements Serializable {
	private static final long serialVersionUID = 442559499771809760L;

	@Id
	@GeneratedValue
	private int id;
    @ManyToOne(fetch=FetchType.LAZY)
	private Publisher publisher;
    @OneToMany(mappedBy="title")
    @OrderBy("issueNum ASC")
	private List<Issue> issues = new ArrayList<Issue>();
    private String name;
    private String format;


    public Title() {
        setFormat("ongoing");
    }

    public Title(int id, String name) {
        this();
        this.id = id;
        this.name = name;
    }

    public Title(int id, String name, Publisher publisher) {
        this(id, name);
        setPublisher(publisher);
    }

    public String getFormat() {
        return this.format;
    }

    public int getId() {
        return this.id;
    }

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
        return this.name;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public void setName(String title) {
        this.name = title;
    }


    public String toString() {
        return getName() + "\t" + getFormat();
    }

    public Publisher getPublisher() {
        return publisher;
    }

    public void setPublisher(Publisher publisher) {
        this.publisher = publisher;
    }

	public List<Issue> getIssues() {
		return issues;
	}

	public void setIssues(List<Issue> issues) {
		this.issues = issues;
	}

	public void removeIssue(Issue anIssue) {
		this.issues.remove(anIssue);
		anIssue.setTitle(null);
	}
	
	public void addIssue(Issue anIssue) {
		this.issues.add(anIssue);
		anIssue.setTitle(this);
	}

}
