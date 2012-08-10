/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              James Sutherland - initial example
 ******************************************************************************/
package model;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;

/**
 * Represents a corporate insurance policy holder.
 */
@Entity
public class CorporateHolder extends PolicyHolderImpl {
    @OneToOne(fetch=FetchType.LAZY, cascade=CascadeType.ALL)
    //@PrimaryKeyJoinColumn(name="ID", referencedColumnName="POLICY.HOLDER_ID")
    @JoinColumn(name="ID", referencedColumnName="HOLDER_ID", insertable=false, updatable=false)
    private CorporatePolicy policy;

    public CorporatePolicy getPolicy() {
        return policy;
    }

    public void setPolicy(CorporatePolicy policy) {
        this.policy = policy;
    }
}
