/*******************************************************************************
 * Copyright (c) 2010, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     16/02/2011 2.3  Michael O'Brien 
 *          - 337037: initial API and implementation platform to be used for 
 *             distributed EE application research, development and architecture
 ******************************************************************************/  
package org.eclipse.persistence.example.distributed.collatz.model;

import javax.persistence.MappedSuperclass;

/**
 * This class is part of a distributed application framework used to simulate and research
 * concurrency, analytics, management, performance and exception handling.
 * The focus is on utilizing JPA 2.0 as the persistence layer for scenarios involving
 * multicore, multithreaded and multiuser distributed memory L1 persistence applications.
 * The secondary focus is on exercising Java EE6 API to access the results of this distributed application.
 * 
 * @see http://bugs.eclipse.org/337037
 * @see http://wiki.eclipse.org/EclipseLink/Examples/Distributed
 * @author Michael O'Brien
 * @since EclipseLink 2.3
 */
@MappedSuperclass
public abstract class DistributedProcessor extends Processor {
    private static final long serialVersionUID = -8570686397938308254L;
    // Caused by: org.hibernate.MappingException: Could not determine type for: java.util.Collection, for columns: [org.hibernate.mapping.Column(ip)]
    // TODO: use JPA2 ElementCollection
    /*
    @BasicCollection(valueColumn=@Column(name="IP")) // JPA 2.0 only
    private Collection<Integer> ip;

    public Collection<Integer> getIp() {
        return ip;
    }

    public void setIp(Collection<Integer> ip) {
        this.ip = ip;
    }
    */
}
