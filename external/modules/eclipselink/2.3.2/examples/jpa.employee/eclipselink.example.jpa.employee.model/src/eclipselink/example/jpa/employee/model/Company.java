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
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.QueryHint;

import org.eclipse.persistence.annotations.Cache;
import org.eclipse.persistence.annotations.CacheType;
import org.eclipse.persistence.config.HintValues;
import org.eclipse.persistence.config.QueryHints;

/**
 * Company entity defining the company an {@link Employee} works for and the
 * company a {@link User} has visibility to see.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
@Entity
@Cache(shared = true, type = CacheType.FULL)
@NamedQuery(name = "Company.findAll", query = "SELECT c FROM Company c ORDER BY c.name", 
        hints = @QueryHint(name = QueryHints.QUERY_RESULTS_CACHE, value = HintValues.TRUE))
public class Company {

    @Id
    @Column(name = "CC")
    private String code;

    private String name;

    public Company() {
        super();
    }

    public Company(String arg2, String string) {
        this.code = arg2;
        this.name = string;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String toString() {
        return "Company(" + getCode() + " - " + getName() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj instanceof Company) {
            return ((Company) obj).getCode().equals(getCode());
        }
        return super.equals(obj);
    }
}
