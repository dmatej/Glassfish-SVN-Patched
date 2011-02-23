/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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


package org.glassfish.fighterfish.sample.uas.ejbservice;

import java.util.List;

import javax.ejb.Local;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.glassfish.fighterfish.sample.uas.api.UserAuthService;
import org.glassfish.fighterfish.sample.uas.entities.LoginAttempt;
import org.glassfish.fighterfish.sample.uas.entities.UserCredential;

/**
 * Session Bean implementation class UserAuthServiceEJB
 */
@Stateless
@Local( { UserAuthService.class })
public class UserAuthServiceEJB implements UserAuthService {

	@PersistenceContext
	private EntityManager em;
	
    /**
     * Default constructor. 
     */
    public UserAuthServiceEJB() {
        // TODO Auto-generated constructor stub
    }

	@Override
	public boolean login(@NotNull @Size(min=3) String name, @NotNull @Size(min=3) String password) {
		UserCredential uc = em.find(UserCredential.class, name);
		boolean result = (uc != null && password.equals(uc.getPassword()));
		log("Logging in (" + name + ", " + password + ")");
                if (uc != null) {
                    // Create a LoginAttempt only for existing users.
		    LoginAttempt attempt = new LoginAttempt();
		    attempt.setSuccessful(result);
		    attempt.setUserCredential(uc);
		    em.persist(attempt);
                }
		return result;
	}

	@Override
	public boolean register(@NotNull @Size(min=3) String name, @NotNull @Size(min=3) String password) {
		UserCredential uc = em.find(UserCredential.class, name);
		if (uc != null) return false;
		uc = new UserCredential();
		uc.setName(name);
		uc.setPassword(password);
		em.persist(uc);
		log("Registering (" + name + ", " + password + ")");
		return true;
	}

	@Override
	public String getReport() {
		List<LoginAttempt> attempts = em.createNamedQuery("LoginAttempt.findAll").getResultList();
		log("Number of entries found: " + attempts.size());
		StringBuilder report = new StringBuilder();
		for (LoginAttempt attempt : attempts) {
			report.append(attempt).append("\n");
		}
		return report.toString();
	}

	private void log(String msg) {
		System.out.println("UserAuthServiceEJB: " + msg);
	}
}
