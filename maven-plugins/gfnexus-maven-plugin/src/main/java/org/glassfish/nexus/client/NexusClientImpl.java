/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012-2013 Oracle and/or its affiliates. All rights reserved.
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.MessageProcessingException;
import javax.ws.rs.client.ClientException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import org.glassfish.nexus.client.beans.ContentItem;
import org.glassfish.nexus.client.beans.ContentItems;
import org.glassfish.nexus.client.beans.Failures;
import org.glassfish.nexus.client.beans.MavenArtifactInfo;
import org.glassfish.nexus.client.beans.MavenInfo;
import org.glassfish.nexus.client.beans.Repo;
import org.glassfish.nexus.client.beans.RepoDetail;
import org.glassfish.nexus.client.beans.RepoDetails;
import org.glassfish.nexus.client.beans.Repos;
import org.glassfish.nexus.client.beans.StagingOperationRequest;
import org.glassfish.nexus.client.beans.StagingOperationRequestData;
import org.glassfish.nexus.client.beans.StagingProfile;
import org.glassfish.nexus.client.beans.StagingProfileRepo;
import org.glassfish.nexus.client.beans.StagingProfileRepos;
import org.glassfish.nexus.client.beans.StagingProfiles;
import org.glassfish.nexus.client.logging.CustomHandler;
import org.glassfish.nexus.client.logging.DefaultNexusClientHandler;
import org.glassfish.nexus.client.logging.DefaultRestHandler;

/**
 *
 * @author Romain Grecourt
 */
public class NexusClientImpl implements NexusClient {

    private RestClient restClient;
    private String nexusUrl;

    private static final String REPOSITORY_GROUP_PATH = "service/local/repo_groups/";
    private static final String REPOSITORIES_PATH = "service/local/repositories";
    private static final String STAGING_REPO_BULK_PATH = "service/local/staging/bulk/";
    private static final String PROFILES_PATH = "/service/local/staging/profiles";
    private static final String PROFILES_REPOS_PATH = "/service/local/staging/profile_repositories";
    private static final String SEARCH_PATH = "service/local/lucene/search";

    private static final Logger logger = Logger.getLogger(NexusClientImpl.class.getSimpleName());

    private static NexusClientImpl instance;
    private static List<StagingProfileRepo> stagingProfileRepositories;
    private static HashMap<String,StagingProfileRepo> stagingProfileRepositoriesMap;

    public static NexusClient init(RestClient restClient, String nexusUrl, CustomHandler loggerHandler){
        logger.setUseParentHandlers(false);
        logger.addHandler(loggerHandler);
        instance = new NexusClientImpl(restClient, nexusUrl);

        return instance;
    }

    private static enum Operation {
        close, promote, drop
    }

    public static NexusClient getInstance(){
        return instance;
    }

    private NexusClientImpl(RestClient restClient, String nexusUrl) {
        this.restClient = restClient;
        this.nexusUrl = nexusUrl;
        refresh();
    }
    
    WebTarget target(String path){
        return restClient.getClient().target(nexusUrl).path(path);
    }

    Builder request(String path){
        return target(path).request(MediaType.APPLICATION_JSON);
    }

    private Response get(String path) throws NexusClientException {
        
        try {
            return request(path).get();
        } catch (ClientException ex) {
            throw new NexusClientException(ex);
        }
    }

    private void refresh() throws NexusClientException {

        stagingProfileRepositories = Arrays.asList(
                ((StagingProfileRepos) handleResponse(
                    get(PROFILES_REPOS_PATH),
                    StagingProfileRepos.class)).getData());
        stagingProfileRepositoriesMap = new HashMap<String, StagingProfileRepo>();
        for (StagingProfileRepo profileRepo : stagingProfileRepositories) {
            stagingProfileRepositoriesMap.put(profileRepo.getRepositoryId(), profileRepo);
        }
    }

    private static String checksum(File datafile) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA1");
            FileInputStream fis = new FileInputStream(datafile);
            byte[] dataBytes = new byte[1024];

            int nread;
            while ((nread = fis.read(dataBytes)) != -1) {
                md.update(dataBytes, 0, nread);
            }

            byte[] mdbytes = md.digest();

            //convert the byte to hex format
            StringBuilder sb = new StringBuilder("");
            for (int i = 0; i < mdbytes.length; i++) {
                sb.append(Integer.toString((mdbytes[i] & 0xff) + 0x100, 16).substring(1));
            }

            return sb.toString();
        } catch (NoSuchAlgorithmException ex) {
            throw new NexusClientException(ex);
        } catch (IOException ex) {
            throw new NexusClientException(ex);
        }
    }

    private Response stagingOperation(
            Operation op,
            String[] repoIds,
            String profileGroup)
            throws NexusClientException {

        if (repoIds == null || repoIds.length == 0) {
            throw new NexusClientException(
                    "repoId list is null or empty, can't perform a staging operation");
        }
        
        try {
            Response response = request(STAGING_REPO_BULK_PATH + "/" + op).post(
                        Entity.entity(new StagingOperationRequestData(
                            new StagingOperationRequest(repoIds, profileGroup, op + " ..."))
                            ,MediaType.APPLICATION_JSON));
            refresh();
            return response;
        } catch (ClientException ex) {
            throw new NexusClientException(ex);
        }
    }

    private static Object handleResponse(Response r, Class c)
            throws NexusClientException {

        // 400 means failure entity
        if (r.getStatus() == 400) {
            try {
                throw new NexusFailureException(r.readEntity(Failures.class));
            } catch (MessageProcessingException ex) {
                throw new NexusClientException(ex);
            }
        }

        // something is wrong
        if (r.getStatus() >= 299) {
            throw new NexusClientException(
                    "http return code (" + r.getStatus() + ")");
        }

        if (c != null) {
            try {
                return r.readEntity(c);
            } catch (MessageProcessingException ex) {
                throw new NexusClientException(ex);
            }
        }
        return null;
    }

    public Repo getStagingRepo(String repoName) throws NexusClientException {
        return ((Repos) handleResponse(get(REPOSITORIES_PATH+"/"+repoName),Repos.class)).getData();
    }

    public Set<Repo> getGroupTree(String repoId){
        Set<Repo> repos = new HashSet<Repo>();
        for(ContentItem item : ((ContentItems) handleResponse(
                get(PROFILES_REPOS_PATH+"/tree/"+repoId),ContentItems.class)).getData()){
            String[] tokens = item.getResourceURI().split("/");
            repos.add(new Repo(stagingProfileRepositoriesMap.get(tokens[tokens.length-1])));
        }
        return repos;
    }

    public StagingProfileRepo getStagingProfileRepo(String repoId){
        return stagingProfileRepositoriesMap.get(repoId);
    }

    public void closeStagingRepo(String... repoIds) throws NexusClientException {
        logger.info(" ");
        logger.log(Level.INFO,
                "-- closing {0} --",
                Arrays.toString(repoIds));
        handleResponse(stagingOperation(Operation.close, repoIds, null), null);
    }

    public void dropStagingRepo(String... repoIds) throws NexusClientException {
        logger.info(" ");
        logger.log(Level.INFO,
                "-- droping {0} --",
                Arrays.toString(repoIds));
        handleResponse(stagingOperation(Operation.drop, repoIds, null), null);
    }

    public Repo promoteStagingRepo(String promotionProfile,String... repoIds)
            throws NexusClientException {

        logger.info(" ");
        logger.log(Level.INFO,
                "-- searching for the promotion profile id of \"{0}\" --",
                promotionProfile);

        for (StagingProfile profile : ((StagingProfiles) handleResponse(
                get(PROFILES_PATH),
                StagingProfiles.class))
                .getData()) {

            if (profile.getMode().equals("GROUP")) {
                if (profile.getName().equals(promotionProfile)) {

                    logger.info(" ");
                    logger.log(Level.INFO,
                            "-- promoting {0} with promotion profile \"{1}\" --",
                            new Object[]{Arrays.toString(repoIds), promotionProfile});

                    // promote
                    handleResponse(
                            stagingOperation(
                            Operation.promote,
                            repoIds,
                            profile.getId()), null);

                    // search the promoted repository
                    StagingProfileRepo repo =
                            stagingProfileRepositoriesMap.get(repoIds[0]);
                    if (repo != null) {
                        return new Repo(stagingProfileRepositoriesMap.get(
                                repo.getParentGroupId()));
                    }

                    throw new NexusClientException(
                            "unable to find the promoted repository after promotion");
                }
            }
        }

        throw new NexusClientException(
                "unable to find the following promotion profile\""
                + promotionProfile
                + "\"");
    }

    private void scrubRepo(String repoId, String path, Set<MavenArtifactInfo> artifacts)
            throws NexusClientException {
        
        StringBuilder sb = new StringBuilder(REPOSITORIES_PATH);
        sb.append('/');
        sb.append(repoId);
        sb.append('/');
        sb.append("content");
        sb.append('/');
        String root = sb.toString();
        ContentItems content =
                (ContentItems) handleResponse(
                    request(sb.append(path).toString()).get()
                    , ContentItems.class);

        for(ContentItem item : content.getData()){
            // if file
            if(item.getLeaf().booleanValue()){
                if(!item.getRelativePath().endsWith("metadata.xml")
                        && !item.getRelativePath().endsWith("archetype-catalog.xml")
                        && !item.getRelativePath().endsWith("asc")
                        && !item.getRelativePath().endsWith("sha1")
                        && !item.getRelativePath().endsWith("md5")){

                    MavenArtifactInfo artifact =
                            ((MavenInfo) handleResponse(
                                target(root+"/"+item.getRelativePath())
                                    .queryParam("describe", "maven2")
                                    .request(MediaType.APPLICATION_JSON).get()
                                ,MavenInfo.class)).getData()[0];

                    logger.log(Level.INFO, "found {0}", artifact);
                    artifacts.add(artifact);
                }
            } else {
                // if directory
                if(item.getSizeOnDisk().intValue() == -1){
                    scrubRepo(repoId, item.getRelativePath(), artifacts);
                }
            }
        }
    }

    public boolean existsInRepoGroup(String repoGroup, MavenArtifactInfo artifact)
            throws NexusClientException {

        ContentItem[] items = ((ContentItems) handleResponse(
                get(REPOSITORY_GROUP_PATH+repoGroup+"/"+artifact.getRepositoryRelativePath()),
                ContentItems.class)).getData();
        return (items!= null && items.length > 0);
    }

    public Set<MavenArtifactInfo> getArtifactsInRepo(String repoId) 
            throws NexusClientException {
        
        logger.info(" ");
        logger.log(Level.INFO
                , "-- retrieving full content of repository [{0}] --"
                , repoId);
        
        Set<MavenArtifactInfo> artifacts = new HashSet<MavenArtifactInfo>();
        scrubRepo(repoId, "", artifacts);
        return artifacts;
    }

    public Repo getStagingRepo(String stagingProfile,MavenArtifactInfo refArtifact)
            throws NexusClientException {

        String refChecksum = checksum(refArtifact.getFile());

        logger.info(" ");
        logger.info("-- searching for the staging repository --");

        for (StagingProfileRepo repo : stagingProfileRepositories) {
            if (repo.getProfileName().equals(stagingProfile)
                    && repo.getUserId().equals(restClient.getUsername())) {

                // get checksum from repo
                StringBuilder sb = new StringBuilder(REPOSITORIES_PATH);
                sb.append('/');
                sb.append(repo.getRepositoryId());
                sb.append('/');
                sb.append("content");
                sb.append('/');
                sb.append(refArtifact.getRepositoryRelativePath());
                sb.append(".sha1");
                String checksum = request(sb.toString()).get().readEntity(String.class);

                if(refChecksum.equals(checksum)){

                    logger.log(Level.INFO
                            ,"found staging repository: [{0}]"
                            , new Object[]{repo.getRepositoryId()});

                    return getStagingRepo(repo.getRepositoryId());
                }

                logger.log(Level.INFO
                        ,"[{0}] does not contain the ref artifact"
                        , new Object[]{repo.getRepositoryId()});
            }
        }

        throw new NexusClientException(
                "unable to find an open staging repository for staging profile \""
                + stagingProfile
                + "\" and ref artifact \""
                + refArtifact
                + "\"");
    }
    
    public Repo getHostedRepo(File f)
            throws NexusClientException {

            RepoDetail[] results = ((RepoDetails) handleResponse(
                    target(SEARCH_PATH)
                    .queryParam("sha1", checksum(f))
                    .request(MediaType.APPLICATION_JSON).get(),
                    RepoDetails.class)).getRepoDetails();

            if (results != null) {
                for (RepoDetail detail : results) {
                    if (!detail.isGroup()) {
                        return getStagingRepo(detail.getRepositoryId());
                    }
                }
            }
            return null;
    }

    public static void main (String[] args){
        NexusClient nexusClient = NexusClientImpl.init(
                new RestClient(
                    null, 0,
                    "user", "password",
                    true,
                    new DefaultRestHandler())
                ,"https://maven.java.net"
                ,new DefaultNexusClientHandler());

        Repo repo = nexusClient.getStagingRepo(
                "org-glassfish",
                new MavenArtifactInfo("groupId", "artifactId", "version", "classifier", "extension", null));
        repo.close();
    }
}