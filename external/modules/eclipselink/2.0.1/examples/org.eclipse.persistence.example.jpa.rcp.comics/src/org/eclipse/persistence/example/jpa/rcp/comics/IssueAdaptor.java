/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle., Eclipse Foundation All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     tware, ssmith - 1.0 - RCP Demo
 ******************************************************************************/  
package org.eclipse.persistence.example.jpa.rcp.comics;

import org.eclipse.persistence.example.jpa.comics.model.annotated.Issue;

public class IssueAdaptor extends AbstractAdapter {

    public String getLabel(Object o) {
        return String.valueOf(((Issue) o).getIssueNum());
    }

    public Object[] getChildren(Object o) {
        return new Object[0];
    }

}