/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - TODO
 ******************************************************************************/
package eclipselink.example.jpa.employee.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.eclipse.persistence.annotations.Cache;
import org.eclipse.persistence.annotations.CacheType;

/**
 * Represents an application user
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
@Entity
@Table(name="EMP_USER")
@Cache(shared=true, type=CacheType.FULL)
public class User {

    @Id
    @Column(name = "U_ID")
    private String id;
    
    /**
     * TODO: Store encrypted value
     */
    @Column(name="PWD")
    private String password;
    
    @Column(name="F_NAME")
    private String firstName;
    
    @Column(name="L_NAME")
    private String lastName;
    
    /**
     * Company this user has access to. For administrators this value is null.
     */
    @ManyToOne(fetch=FetchType.LAZY)
    @JoinColumn(name="CC")
    private Company company;
    
    @Column(name="SYS_ADMIN")
    private boolean administrator = false;
    
    @Column(name="C_ADMIN")
    private boolean companyAdmin = false;

    public User() {
        
    }
    
    public User(String userid, String firstName, String lastName) {
        this();
        this.id = userid;
        this.firstName = firstName;
        this.lastName = lastName;
    }
    
    /**
     * @return the administrator
     */
    public boolean isAdministrator() {
        return administrator;
    }

    /**
     * @param administrator the administrator to set
     */
    public void setAdministrator(boolean administrator) {
        this.administrator = administrator;
    }

    public String getId() {
        return this.id;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the password
     */
    public String getPassword() {
        return password;
    }

    /**
     * @param password the password to set
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * @return the firstName
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName the firstName to set
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return the lastName
     */
    public String getLastName() {
        return lastName;
    }

    /**
     * @param lastName the lastName to set
     */
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    /**
     * @return the company
     */
    public Company getCompany() {
        return company;
    }

    /**
     * @param company the company to set
     */
    public void setCompany(Company company) {
        this.company = company;
    }
    
    public boolean isCompanyAdmin() {
        return companyAdmin;
    }

    public void setCompanyAdmin(boolean companyAdmin) {
        this.companyAdmin = companyAdmin;
    }

    public String toString() {
        return "User(" + getId() + ")";
    }

    public boolean validate(String pwd) {
        if (this.password == null || this.password.isEmpty()) {
            return pwd == null || pwd.isEmpty(); 
        }
        return this.password.equals(pwd);
    }
}
