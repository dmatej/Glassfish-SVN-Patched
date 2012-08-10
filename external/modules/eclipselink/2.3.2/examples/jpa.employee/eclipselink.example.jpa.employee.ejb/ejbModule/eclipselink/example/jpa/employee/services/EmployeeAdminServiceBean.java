/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services;

import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;

import org.eclipse.persistence.jpa.JpaEntityManager;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.logging.SessionLog;
import org.eclipse.persistence.sessions.DatabaseLogin;
import org.eclipse.persistence.sessions.DatabaseSession;
import org.eclipse.persistence.sessions.Project;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.sessions.server.ServerSession;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.model.util.Sample;
import eclipselink.example.jpa.employee.persistence.EmployeeCompanyCriteria;

@Stateless
public class EmployeeAdminServiceBean extends EmployeeBaseService implements EmployeeAdminService {

    @Override
    public User login(String userId, String password) {
        EntityManager em = getEntityManager();

        Server session = JpaHelper.getEntityManager(em).getServerSession();
        User user = EmployeeCompanyCriteria.findUser(session, userId);
        if (user != null && user.validate(password)) {
            // Remove password from detached instance being returned to web
            // tier.
            user.setPassword(null);
            return user;
        }
        return null;
    }

    /**
     * This method must be called within a transactional method of the session
     * bean where the injected {@link EntityManager} will be set and be wrapping
     * a concrete EclipseLink implementation that can be accessed through
     * getDelegate().
     */
    private DatabaseSession createDirectSession() {
        JpaEntityManager emImpl = JpaHelper.getEntityManager(getEntityManager());

        if (emImpl != null) {
            ServerSession ss = emImpl.getServerSession();

            Project project = (Project) ss.getProject().clone();

            DatabaseLogin login = new DatabaseLogin();
            login.useOracleThinJDBCDriver();
            login.setDatabaseURL("localhost:1521:ORCL2");
            login.setUserName("scott");
            login.setPassword("tiger");

            project.setDatasourceLogin(login);

            DatabaseSession session = project.createDatabaseSession();

            session.getSessionLog().setLevel(SessionLog.FINE);
            session.getSessionLog().setLevel(SessionLog.WARNING, SessionLog.EJB_OR_METADATA);
            session.getSessionLog().setShouldPrintConnection(false);
            session.getSessionLog().setShouldPrintDate(false);
            session.getSessionLog().setShouldPrintThread(false);

            session.login();
            return session;
        }

        throw new IllegalStateException("Could not access shared server session ");
    }

    public void createSchema(User user) {
        DatabaseSession session = createDirectSession();
        SchemaManager sm = new SchemaManager(session);
        sm.createDefaultTables(true);
        session.logout();
    }

    public void replaceSchema() {
        DatabaseSession session = createDirectSession();
        SchemaManager sm = new SchemaManager(session);
        sm.replaceDefaultTables(false, true);
        sm.replaceSequences();
        session.logout();
    }

    public void populate(Company company) {
        EntityManager em = getEntityManager();

        Company dbCompany = em.find(Company.class, company.getCode());
        if (dbCompany != null) {
            new Sample(dbCompany).persistAll(em, false);
        }
    }

    public List<Company> createCompany(String code, String name) {
        EntityManager em = getEntityManager();

        Company existing = em.find(Company.class, code);

        if (existing != null) {
            existing.setName(name);
        } else {
            Company newCompany = new Company();
            newCompany.setCode(code);
            newCompany.setName(name);
            em.persist(newCompany);
        }
        em.flush();

        return getCompanies();
    }

    @SuppressWarnings("unchecked")
    public List<Company> getCompanies() {
        EntityManager em = getEntityManager();

        try {
            return em.createQuery("SELECT c FROM Company c ORDER BY c.code").getResultList();
        } finally {
            em.close();
        }
    }

}
