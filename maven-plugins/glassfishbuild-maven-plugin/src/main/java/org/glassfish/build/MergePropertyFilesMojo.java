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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * merges two property files properly
 *
 * @goal merge-property-files
 *
 * @author Romain Grecourt
 */
public class MergePropertyFilesMojo extends AbstractMojo {

 
    /**
     * @parameter expression="${merge.property.files.outputFile}" default-value="${project.build.directory}/merged.properties"
     */
    protected File outputFile;
   
    /**
     * @parameter expression="${merge.property.files.inputFile1}"
     */
    protected File inputFile1;
   
    /**
     *
     * @parameter expression="${merge.property.files.inputFile2}"
     */
    protected File inputFile2;  
   
    /**
     *
     * @parameter expression="${merge.property.files.skip}" default-value="false"
     */
    protected Boolean skip;

    public void execute() throws MojoExecutionException, MojoFailureException {
        if(skip.booleanValue()){
            getLog().info("skipping...");
            return;
        }
        
        if(!inputFile1.exists() || !inputFile2.exists()){
            throw new MojoFailureException("input file error");
        }
        
        try {
            BufferedReader br1 = new BufferedReader(new FileReader(inputFile1));
            FileReader fr2 = new FileReader(inputFile2);
            FileWriter fw = new FileWriter(outputFile);

            String line;
            StringBuilder sb = new StringBuilder();
            
            // get copyright of inputFile1
            while ((line = br1.readLine()) != null && line.startsWith("#")) {
                sb.append(line);
                sb.append('\n');
            }
            
            Properties props = new Properties();
            props.load(br1);
            props.load(fr2);
            props.store(fw, sb.toString());
        } catch (IOException ex) {
            throw new MojoExecutionException(ex.getMessage(), ex);
        }
    }
}