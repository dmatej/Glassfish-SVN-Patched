/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
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
package org.glassfish.build.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.maven.model.Repository;
import org.apache.maven.model.building.FileModelSource;
import org.apache.maven.model.building.ModelSource;
import org.apache.maven.model.resolution.ModelResolver;
import org.apache.maven.model.resolution.UnresolvableModelException;
import org.apache.maven.repository.internal.ArtifactDescriptorUtils;
import org.sonatype.aether.RepositorySystem;
import org.sonatype.aether.RepositorySystemSession;
import org.sonatype.aether.artifact.Artifact;
import org.sonatype.aether.repository.RemoteRepository;
import org.sonatype.aether.resolution.ArtifactRequest;
import org.sonatype.aether.resolution.ArtifactResolutionException;
import org.sonatype.aether.util.artifact.DefaultArtifact;

/**
 * Resolves an artifact even from remote repository during resolution of the model.
 *
 * The repositories are added to the resolution chain as found during processing of the POM file. Repository is added only if
 * there is no other repository with same id already defined.
 *
 */
public class MavenModelResolver implements ModelResolver {

    private List<RemoteRepository> repositories;
    private Set<String> repositoryIds;

    private RepositorySystem system;
    private RepositorySystemSession session;

    public MavenModelResolver(
            RepositorySystem system,
            RepositorySystemSession session,
            List<RemoteRepository> remoteRepositories) {
        this.system = system;
        this.session = session;
        this.repositories = new ArrayList<RemoteRepository>(remoteRepositories);
        this.repositoryIds = new HashSet<String>();

        for (RemoteRepository repository : repositories) {
            repositoryIds.add(repository.getId());
        }

    }

    private MavenModelResolver(MavenModelResolver clone) {
        this.system = clone.system;
        this.session = clone.session;
        this.repositories = new ArrayList<RemoteRepository>(clone.repositories);
        this.repositoryIds = new HashSet<String>(clone.repositoryIds);
    }

    @Override
    public void addRepository(Repository repository) {
        if (repositoryIds.contains(repository.getId())) {
            return;
        }
        if (!repositoryIds.add(repository.getId())) {
            return;
        }

        List<RemoteRepository> newRepositories =
                Collections.singletonList(
                ArtifactDescriptorUtils.toRemoteRepository(repository));

        repositoryIds.add(repository.getId());
        repositories.addAll(newRepositories);
    }

    @Override
    public ModelResolver newCopy() {
        return new MavenModelResolver(this);
    }

    @Override
    public ModelSource resolveModel(String groupId, String artifactId, String version) throws UnresolvableModelException {
        Artifact pomArtifact = new DefaultArtifact(
                groupId,
                artifactId,
                "",
                "pom",
                version);
        try {
            ArtifactRequest request = new ArtifactRequest(
                    pomArtifact,
                    repositories,
                    null);
            pomArtifact = system.resolveArtifact(session, request).getArtifact();

        } catch (ArtifactResolutionException e) {
            throw new UnresolvableModelException(
                    String.format("Failed to resolve POM for %s:%s:%s due to %s",
                    groupId, artifactId, version, e.getMessage()),
                    groupId, artifactId,
                    version, e);
        }
        return new FileModelSource(pomArtifact.getFile());
    }
}