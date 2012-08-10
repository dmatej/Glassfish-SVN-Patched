/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - TODO 
 ******************************************************************************/
package eclipselink.example.jpa.employee;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.User;

public class CompanyAdminRepository {

    protected PersistenceService persistence;

    public CompanyAdminRepository(PersistenceService persistence) {
        this.persistence = persistence;
    }

    public PersistenceService getPersistence() {
        return persistence;
    }

    // TODO
    public User createUser(String userid, String password, String firstName, String lastName) {
        throw new RuntimeException("NOT YET IMPLEMENTED");
    }

    public List<Company> getCompanies() {
        EntityManager em = getPersistence().createEntityManager();

        try {
            return em.createNamedQuery("Company.findAll", Company.class).getResultList();
        } finally {
            em.close();
        }
    }

    public Company findCompany(String code) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            return em.find(Company.class, code);
        } finally {
            em.close();
        }
    }

    public void persist(Company company) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.persist(company);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public void merge(Company company) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.merge(company);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public void remove(Company company) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.remove(company);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public List<Company> companySearch(String pattern) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            return em.createQuery("SELECT c FROM Company c WHERE c.name LIKE :PATTERN", Company.class).
            setParameter("PATTERN", pattern == null ? "%" : pattern).getResultList();
        } finally {
            em.close();
        }
    }

    /**
     * Lookup a {@link User} based on it's {@link IdentifierBag#} and return a
     * view of it.
     */
    public User findUser(String id) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            return em.find(User.class, id);
        } finally {
            em.close();
        }
    }

    public void persist(User user) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.persist(user);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }
    public void merge(User user) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.merge(user);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }
    public void remove(User user) {
        EntityManager em = getPersistence().createEntityManager();

        try {
            em.getTransaction().begin();
            em.remove(user);
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    public List<User> searchUsers(String pattern) {
        EntityManager em = getPersistence().createEntityManager();
        
        try {
            TypedQuery<User> query = em.createQuery("SELECT u FROM User u WHERE u.lastName LIKE :PATTERN", User.class);
            query.setParameter("PATTERN", pattern == null ? "%" : pattern);
            return query.getResultList();
        } finally {
            em.close();
        }
    }
}
