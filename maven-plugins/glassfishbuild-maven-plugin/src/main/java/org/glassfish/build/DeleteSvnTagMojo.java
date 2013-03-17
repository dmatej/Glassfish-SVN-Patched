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
package org.glassfish.build;

import java.net.MalformedURLException;
import java.net.URL;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.scm.manager.NoSuchScmProviderException;
import org.apache.maven.scm.manager.ScmManager;
import org.apache.maven.scm.provider.svn.svnjava.repository.SvnJavaScmProviderRepository;
import org.apache.maven.scm.repository.ScmRepository;
import org.apache.maven.scm.repository.ScmRepositoryException;
import org.apache.maven.settings.Server;
import org.apache.maven.settings.Settings;
import org.tmatesoft.svn.core.SVNCommitInfo;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.auth.BasicAuthenticationManager;
import org.tmatesoft.svn.core.wc2.SvnGetInfo;
import org.tmatesoft.svn.core.wc2.SvnOperationFactory;
import org.tmatesoft.svn.core.wc2.SvnRemoteDelete;
import org.tmatesoft.svn.core.wc2.SvnTarget;

/**
 * Delete the svn tag corresponding to the next version to be released!
 *
 * @goal delete-svn-tag
 * @aggregator
 * @author Romain Grecourt
 */
public class DeleteSvnTagMojo extends AbstractMojo {

    /**
     * @parameter default-value="${project}"
     * @required
     * @readonly
     */
    private MavenProject project;
    
    /**
     * The system settings for Maven. This is the instance resulting from
     * merging global and user-level settings files.
     *
     * @parameter expression="${settings}"
     * @readonly
     * @required
     */
    protected Settings settings;
    
    /**
     * @parameter expression="${delete.svn.tag.base}"
     */
    private String tagBase;
    /**
     * @parameter expression="${delete.svn.tag.name}"
     * @required
     */
    private String tagName;
    /**
     * The message prefix to use for all SCM changes.
     *
     * @parameter expression="${commitPrefix}" default-value="[glassfishbuild-maven-plugin] delete-tag - "
     */
    private String commitPrefix;
    /**
     * The SCM manager.
     *
     * @component
     */
    private ScmManager scmManager;
    private SvnTarget target = null;
    private SvnOperationFactory svnOperationFactory = null;

    public void execute() throws MojoExecutionException, MojoFailureException {
        init();

        if (tagExists()) {

            getLog().info("deleting " + target.getPathOrUrlString());
            SvnRemoteDelete delete = svnOperationFactory.createRemoteDelete();
            delete.addTarget(target);
            delete.setCommitMessage(commitPrefix + tagName);
            try {
                SVNCommitInfo commitInfo = delete.run();
                getLog().debug("committed revision " + commitInfo.getNewRevision());
            } catch (SVNException ex) {
                throw new MojoExecutionException("failed to remove tag " + target.getPathOrUrlString(), ex);
            }
        } else {
            getLog().info(target.getPathOrUrlString() + " does not exist");
        }
    }

    private boolean tagExists() {
        SvnGetInfo info = svnOperationFactory.createGetInfo();
        info.setSingleTarget(target);
        try {
            info.run();
        } catch (SVNException ex) {
            return false;
        }
        return true;
    }

    private void init() throws MojoExecutionException {
        try {
            // make sure we use javasvn
            scmManager.setScmProviderImplementation("svn", "javasvn");
            
            // get our provider repo configured by maven :)
            ScmRepository repo = scmManager.makeScmRepository(project.getScm().getDeveloperConnection());
            
            // make sure we are dealing with svn repo
            if(!repo.getProvider().equals("svn")){
                getLog().warn("only svn SCM is supported");
                return;
            }
            
            // create and configure our operation factory
            SvnJavaScmProviderRepository providerRepo = (SvnJavaScmProviderRepository) repo.getProviderRepository();
            svnOperationFactory = new SvnOperationFactory();
            svnOperationFactory.setOptions(providerRepo.getClientManager().getOptions());

            // build the tag URL
            StringBuilder tagStrUrl = new StringBuilder();
            if (tagBase != null && !tagBase.isEmpty()) {
                tagStrUrl.append(tagBase);
            } else {
                tagStrUrl.append(providerRepo.getTagBase());
            }
            tagStrUrl.append('/');
            tagStrUrl.append(tagName);

            URL tagURL = new URL(tagStrUrl.toString());
            SVNURL svnTagURL = SVNURL.create(
                    tagURL.getProtocol(),
                    tagURL.getUserInfo(),
                    tagURL.getHost(),
                    tagURL.getPort(),
                    tagURL.getPath(),
                    false);
            target = SvnTarget.fromURL(svnTagURL);
            
            // add credentials from settings.xml
            Server server = settings.getServer(tagURL.getHost());
            if (server != null) {
                svnOperationFactory.setAuthenticationManager(
                        new BasicAuthenticationManager(
                        server.getUsername(),
                        server.getPassword()));
            }

        } catch (SVNException ex) {
            throw new MojoExecutionException("init failed", ex);
        } catch (ScmRepositoryException ex) {
            throw new MojoExecutionException("init failed", ex);
        } catch (NoSuchScmProviderException ex) {
            throw new MojoExecutionException("init failed", ex);
        } catch (MalformedURLException ex) {
            throw new MojoExecutionException("init failed", ex);
        }
    }
}