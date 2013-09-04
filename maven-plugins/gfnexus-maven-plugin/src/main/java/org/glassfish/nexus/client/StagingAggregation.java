/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package org.glassfish.nexus.client;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import org.glassfish.nexus.client.beans.Repo;

/**
 *
 * @author Romain Grecourt
 */
public class StagingAggregation implements StagingOperation {
    private Set<StagingOperation> repos;

    public StagingAggregation() {
        repos = new HashSet<StagingOperation>();
    }

    public StagingAggregation(Repo repo) {
        this();
        if(repo != null){
            repos.add(repo);
        }
    }

    private NexusClientImpl getNexusClient(){
        return ((NexusClientImpl)NexusClientImpl.getInstance());
    }

    public StagingAggregation(Collection<Repo> repos) {
        this();
        repos.addAll(repos);
    }

    public void close() throws NexusClientException {
        getNexusClient().closeStagingRepo("");
    }

    public void drop() throws NexusClientException {
        getNexusClient().dropStagingRepo("");
    }

    public Repo promote(String profile, String desc) throws NexusClientException {
        String[] ids = getIds();
        if(ids != null && ids.length >0){
            return getNexusClient().promoteStagingRepo(profile, desc, getIds());
        }
        return null;
    }

    public StagingAggregation aggregate(StagingOperation repo) throws NexusClientException {
        repos.add(repo);
        return this;
    }

    public String[] getIds() {
        String[] repoIds = null;
        if (!repos.isEmpty()) {
            repoIds = new String[repos.size()];
            Iterator<StagingOperation> it = repos.iterator();
            int i = 0;
            while(it.hasNext()){
                repoIds[i] = ((StagingOperation)it.next()).getIds()[0];
                i++;
            }
        }
        return repoIds;
    }
}
