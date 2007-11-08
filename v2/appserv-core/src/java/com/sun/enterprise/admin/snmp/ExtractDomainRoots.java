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

package com.sun.enterprise.admin.snmp;

//import from DomainRegistry
import com.sun.enterprise.admin.common.domains.registry.DomainRegistry;
import com.sun.enterprise.admin.common.domains.registry.DomainRegistryException;
import com.sun.enterprise.admin.common.domains.registry.DomainEntry;

// jdk imports
import java.lang.*;
import java.io.*;
import java.util.Vector;
import java.util.Arrays;
import java.util.Iterator;

public class ExtractDomainRoots {
	
	public String[] getAllDomainRoots() {
		Vector vt = new Vector();
        String[] domains = null;
        try
        {
            DomainRegistry domainRegistry = DomainRegistry.newInstance();
            Iterator it = domainRegistry.iterator();
            while (it.hasNext())
            {
                DomainEntry entry = (DomainEntry)it.next();
                vt.addElement(entry.getRoot().getPath());
            }
			domains = new String[vt.size()];
			domains = (String[])vt.toArray(domains);
        }
        catch (DomainRegistryException e)
        {
			System.err.println("ExtractDomainRoots: Exception caught, while parsing the domain registry");
			e.printStackTrace();
        }
       	return domains;
	}

	public void writeDomainInfo(String[] domains) {
		String fileName = new String("/tmp/appserv_domainInfo.txt"); 
		try {
			File domainFile = new File(fileName);
			FileWriter domainFileWriter = new FileWriter(domainFile);
			for(int i = 0; i< domains.length; i++)
			{
				domainFileWriter.write(domains[i]);
				domainFileWriter.write("\n");
				System.out.println(domains[i]);
			}
			domainFileWriter.flush();
			domainFileWriter.close();
		}
		catch(IOException e) {
			System.out.println("ExtractDomainRoots: IOException caught while writing domain information to file");
			e.printStackTrace();
		}
		catch(Exception e) {
			System.out.println("ExtractDomainRoots: Exception caught while writing domain information to file");
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		ExtractDomainRoots droot = new ExtractDomainRoots();
		String[] dInfo = droot.getAllDomainRoots();
		droot.writeDomainInfo(dInfo);
	}
}
