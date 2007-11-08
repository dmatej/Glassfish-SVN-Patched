/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
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

package com.sun.enterprise.deployment.util;

import java.util.*;
import java.util.logging.Level;
import com.sun.enterprise.deployment.Application;
import com.sun.enterprise.deployment.BundleDescriptor;
import com.sun.enterprise.deployment.EjbBundleDescriptor;
import com.sun.enterprise.deployment.ApplicationClientDescriptor;
import com.sun.enterprise.deployment.WebBundleDescriptor;
import com.sun.enterprise.deployment.WebComponentDescriptor;
import com.sun.enterprise.deployment.EjbDescriptor;
import com.sun.enterprise.deployment.EjbIORConfigurationDescriptor;
import com.sun.enterprise.deployment.LifecycleCallbackDescriptor;
import com.sun.enterprise.deployment.util.DOLUtils;
import com.sun.enterprise.deployment.util.ModuleDescriptor;
import java.io.File;

/**
 * This class is responsible for validating the loaded DOL classes and 
 * transform some of the raw XML information into refined values used 
 * by the DOL runtime
 *
 * @author Jerome Dochez
 */
public class ApplicationValidator extends EjbBundleValidator 
    implements ApplicationVisitor, EjbBundleVisitor, EjbVisitor {
    

    private Application application;
    
    /**
     * visit an application object
     * @param the application descriptor
     */
    public void accept(Application application) {
        this.application = application;
    }
            
    /**
     * visits an ejb bundle descriptor
     * @param an ejb bundle descriptor
     */
    public void accept(EjbBundleDescriptor bundleDescriptor) {
        
        this.bundleDescriptor = bundleDescriptor;
        super.accept(bundleDescriptor);
        /** set the realm name on each ejb to match the ones on this application
         * this is required right now to pass the stringent CSIv2 criteria 
         * whereby the realm-name for the ejb being authenticated on 
         * has to match the one on the application. We look at the IORConfigurator
         * descriptor
         * @todo: change the csiv2 layer so that it does not look at 
         * IORConfiguratorDescriptor. 
         * @see iiop/security/SecurityMechanismSelector.evaluateClientConformance.
         */
        String rlm = application.getRealm();
        Iterator ejbs = bundleDescriptor.getEjbs().iterator();
        for(; ejbs.hasNext();){
            EjbDescriptor ejb = (EjbDescriptor) ejbs.next();
            Iterator iorconfig = ejb.getIORConfigurationDescriptors().iterator();
            for (;iorconfig.hasNext(); ){
                EjbIORConfigurationDescriptor desc = 
                    (EjbIORConfigurationDescriptor)iorconfig.next();
                desc.setRealmName(rlm);
            }
        }
    }
    
    
    /**
     * visit a web bundle descriptor
     *
     * @param the web bundle descriptor
     */
    public void accept(WebBundleDescriptor descriptor) {
        bundleDescriptor = descriptor;
        ModuleDescriptor md = bundleDescriptor.getModuleDescriptor( );
        // Fix for bug: 4837982
        String uri = md.getArchiveUri( );
        if( ( md.getContextRoot() == null )
         && ( ( uri != null ) && (uri.length() != 0) ) )
         {
            // Case 1: If there is a unix style file separator
            // Example a/b/xxx.war
            int beginIndex = uri.lastIndexOf( "/" );

            // Case 2: If there is a windows style file separator
            // Example a\b\xxx.war
            if( beginIndex < 0 ) {
                beginIndex = uri.lastIndexOf( File.separator );
            }

            // Case 3: No File separator
            // Example xxx.war
            if( beginIndex < 0 ) {
                beginIndex = 0;
            } else {
                // If there is a separator, we need to increment to get the
                // string past the last separator 
                beginIndex++;
            }

            // If the context-root is not specified, AppServer will use
            // the file name with the extension removed as the context
            // root
            
            // NOTE: We can safely assume that the file extension is ".war"
            // So no need to do extra checks
            int endIndex = uri.lastIndexOf( ".war" );
            if (endIndex==-1) {
                return;
            }
            String warFileName = uri.substring( beginIndex, endIndex );
            md.setContextRoot( warFileName );
            if( DOLUtils.getDefaultLogger().isLoggable(Level.INFO) ) {
                DOLUtils.getDefaultLogger().info(
                    "Context Root is not provided by the user, Using ["
                    + warFileName + "] as Context Root" );
            }
        }
    }   
    
    /**
     * visits a appclient descriptor
     * @param appclientdescriptor
     */
    public void accept(ApplicationClientDescriptor appclientdescriptor) {
        bundleDescriptor = appclientdescriptor;        

        // set the default lifecycle callback class
        for (LifecycleCallbackDescriptor next :
            appclientdescriptor.getPreDestroyDescriptors()) {
            next.setDefaultLifecycleCallbackClass(
                appclientdescriptor.getMainClassName());
        }

        for (LifecycleCallbackDescriptor next :
            appclientdescriptor.getPostConstructDescriptors()) {
            next.setDefaultLifecycleCallbackClass(
                appclientdescriptor.getMainClassName());
        }
    }    

    /**
     * visit a web component descriptor
     *
     * @param the web component
     */
    public void accept(WebComponentDescriptor descriptor) {
        computeRuntimeDefault(descriptor);
    }
       
    private void computeRuntimeDefault(WebComponentDescriptor webComp) {
        if (!webComp.getUsesCallerIdentity()) {
            computeRunAsPrincipalDefault(
                webComp.getRunAsIdentity(), webComp.getApplication());
        }
    }
    
    /**
     * @return a vector of EjbDescriptor for this bundle
     */
    protected Collection getEjbDescriptors() {
        if (application!=null) 
            return application.getEjbDescriptors();
        return new HashSet();
    }     
    
    /**
     * @return the Application object if any
     */
    protected Application getApplication() {
        return application;
    }
    
    /**
     * @return the bundleDescriptor we are validating
     */
    protected BundleDescriptor getBundleDescriptor() {
        return bundleDescriptor;
    }    
    
    
}
