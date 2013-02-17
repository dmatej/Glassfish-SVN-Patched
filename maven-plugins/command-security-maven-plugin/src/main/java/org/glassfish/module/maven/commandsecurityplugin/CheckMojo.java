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
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
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
package org.glassfish.module.maven.commandsecurityplugin;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Developer;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

/**
 * Verifies that all inhabitants in the module that are commands also take care
 * of authorization, issuing warnings or failing the build (configurable) if
 * any do not.
 * <p>
 * The mojo has to analyze not only the inhabitants but potentially also 
 * their ancestor classes.  To improve performance across multiple modules in 
 * the same build the mojo stores information about classes known to be commands and
 * classes known not to be commands in the maven session. 
 * 
 * @author tjquinn
 * @goal check
 * @threadSafe
 * @phase process-classes
 * @requiresProject
 * @requiresDependencyResolution compile+runtime
 */
public class CheckMojo extends CommonMojo {
    /**
     * Whether failures are fatal to the build.
     * @parameter
     *   expression="${command-security-maven-plugin.isFailureFatal}"
     *   default-value="true"
     */
    private String isFailureFatal;
    
    /**
     * Path to which to print a violation summary wiki table.  If empty,
     * print no table.
     * @parameter
     *   expression="${command-security-maven-plugin.violationWikiPath}"
     *   default-value=""
     */
    protected String violationWikiPath;
    
    /**
     * Path to properties file listing owners of modules.
     * 
     * The format is (moduleId).owner=(owner name)
     *               (moduleId).notes=(notes) - if any
     * 
     * @parameter 
     *   expression="${command-security-maven-plugin.moduleInfoPath}"
     *   default-value="~/moduleInfo.txt"
     */
    protected String moduleOwnersPath;
    
    private static final String WIKI_INFO_SET = "is-wiki-info-set";
    private static WikiOutputInfo wikiOutputInfo;
    
    private boolean isLastProject() {
        final List<MavenProject> projects = (List<MavenProject>) reactorProjects;
        return project.equals(projects.get(projects.size() - 1));
    }
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        try {
            initWikiOutputInfo();
        } catch (IOException ex) {
            throw new MojoFailureException("Error initializing output file", ex);
        }
        final TypeProcessor typeProcessor = new TypeProcessorImpl(this, session, project, isFailureFatal,
                isCheckAPIvsParse);
        typeProcessor.execute();
        
        final StringBuilder trace = typeProcessor.trace();
        
        if (trace != null) {
            getLog().debug(trace.toString());
        }
        if (typeProcessor.okClassNames() != null) {
            getLog().debug("Command classes with authorization: " + typeProcessor.okClassNames().toString());
        }
        final List<String> offendingClassNames = typeProcessor.offendingClassNames();
        if ( ! offendingClassNames.isEmpty()) {
            if (wikiOutputInfo != null) {
                try {
                    ensureViolationWikiTitleIsPresent();
                } catch (IOException ex) {
                    throw new MojoFailureException("Error opening output file and writing title", ex);
                }
                printViolationWikiRow(typeProcessor.offendingClassNames());
            }
            if (typeProcessor.isFailureFatal()) {
                getLog().error("Following command classes neither provide nor inherit authorization: " + offendingClassNames.toString());
                throw new MojoFailureException("Command class(es) with no authorization");
            } else {
                getLog().warn("Following command classes neither provide nor inherit authorization: " + offendingClassNames.toString());
            }
        }
        if (wikiOutputInfo != null && wikiOutputInfo.wikiWriter != null) {
            wikiOutputInfo.wikiWriter.flush();
        }
        if (isLastProject()) {
            if (wikiOutputInfo != null) {
                wikiOutputInfo.finish();
            }
        }
    }
    
    private void ensureViolationWikiTitleIsPresent() throws IOException {
        if (wikiOutputInfo != null) {
            wikiOutputInfo.ensureInitialized();
        }
    }
    
    private void printViolationWikiRow(final List<String> offendingClassNames) {
        final String output = 
                "| " + project.getGroupId() + ":" + project.getArtifactId() + 
                " |" + project.getName() + 
                " | " + relativeToTop(project.getBasedir()) + 
                " | " + formattedList(offendingClassNames) + 
                " | " + nameOrId(getLead()) +
                " |";
        wikiOutputInfo.wikiWriter.println(output);
    }
    
    private Developer getLead() {
        final List<Developer> devs = (List<Developer>) project.getDevelopers();
        Developer lead = (devs.isEmpty() ? null : devs.get(0));
        for (Developer d : (List<Developer>) project.getDevelopers()) {
            final List<String> roles = d.getRoles();
            if (roles != null && roles.contains("lead")) {
                lead = d;
            }
        }
        return lead;
    }
    
    private String nameOrId(final Developer d) {
        String result = "?";
        if (d != null) {
            if (d.getName() != null) {
                result = d.getName();
            } else if (d.getId() != null) {
                result = d.getId();
            }
        }
        return result;
    }
    private String formattedList(final List<String> strings) {
        final StringBuilder sb = new StringBuilder();
        for (String s : strings) {
            if (sb.length() > 0) {
                sb.append("\\\\\n");
            }
            sb.append(s);
        }
        return sb.toString();
    }
    
    private String relativeToTop(final File f) {
        return new File(session.getExecutionRootDirectory()).toURI().relativize(f.toURI()).toASCIIString();
    }
    
    private void initWikiOutputInfo() throws IOException {
        if (wikiOutputInfo == null) {
            /*
             * Don't set up the violations output if the user didn't ask for it.
             */
            if (violationWikiPath == null || violationWikiPath.isEmpty()) {
                return;
            }
            /*
             * Maven seems to load the mojo's class more than once sometimes, so
             * we can't assume that wikiOutputInfo being null means this is the
             * first time the mojo is being run.
             */
            final Properties p = session.getUserProperties();
            final String infoState = p.getProperty(WIKI_INFO_SET);
            if (infoState == null) {
                wikiOutputInfo = new WikiOutputInfo(true);
                p.setProperty(WIKI_INFO_SET, "unopened");
            } else {
                wikiOutputInfo = new WikiOutputInfo(infoState);
            }
        }
    }
    
    private class WikiOutputInfo {
        File wikiFile = null;
        PrintWriter wikiWriter = null;
        final boolean isNew;
        
        private WikiOutputInfo(final boolean isNew) throws IOException {
            this.isNew = isNew;
            if (violationWikiPath != null && ! violationWikiPath.isEmpty()) {
                wikiFile = new File(session.getExecutionRootDirectory(), violationWikiPath);
            }
        }
        
        private WikiOutputInfo(final String state) throws IOException {
            this(false);
            if ( ! state.equals("unopened")) {
                openWriter();
            }
        }
        
        private void openWriter() throws IOException {
            wikiWriter = new PrintWriter(new FileWriter(wikiFile, ! isNew));
            session.getUserProperties().setProperty(WIKI_INFO_SET, "open");
        }
        
        private void ensureInitialized() throws IOException {
            if (wikiWriter == null) {
                openWriter();
                wikiWriter.println("{table-plus}");
                wikiWriter.println("|| Module ID || Module Name || Path || Classes Needing Attention || Owner ||");
            }
        }
        
        private void finish() {
            if (wikiWriter != null) {
                final String state = session.getUserProperties().getProperty(WIKI_INFO_SET);
                if ( ! "unopened".equals(state)) {
                    wikiWriter.println("{table-plus}");
                    wikiWriter.close();
                }
            }
        }
    }
}
