/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package example;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;

import org.eclipse.persistence.config.QueryHints;

import model.*;

/**
 * Simple query examples.
 */
public class Queries {

    public static void main(String[] args) throws Exception {        
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");
        Populate.populate(emf);
        Queries queries = new Queries();
        queries.clear(emf);
        // TODO: Does not work.
        //queries.readAllPersistentObject(emf);
        queries.readAllPolicyHolders(emf);
        queries.readAllPolicyHoldersCriteria(emf);
        queries.findPolicyHolder(emf);
        // TODO: Does not work.
        //queries.readAllPolicyHoldersJoinFetch(emf);
        //queries.readAllPolicyHoldersBatch(emf);
        queries.clear(emf);
        queries.readAllPersonalHoldersJoinFetch(emf);
        queries.readAllPolicies(emf);
        queries.readAllHealthPolicies(emf);
        queries.readAllVehiclePolicies(emf);
        queries.readAllMotorcyclePolicies(emf);
        queries.readPolicyJoinHolder(emf);
        queries.readAllClaims(emf);
        queries.readAllHealthClaims(emf);
        queries.readAllVehicleClaims(emf);
        queries.readAllMotorcycleClaims(emf);
        // TODO: Does not work.
        //queries.readClaimJoinHolder(emf);
        queries.updateAllPolicyHolders(emf);
        queries.updateAllPolicies(emf);
        queries.updateAllClaims(emf);
        emf.close();
    }

    public List<PolicyHolder> readAllPersistentObject(EntityManagerFactory emf) {
        System.out.println("Reading all persistent objects.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from PersistentObject p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }
    
    public List<PolicyHolder> readAllPolicyHolders(EntityManagerFactory emf) {
        // TODO: This should not select from the abstract class, but currently does.
        System.out.println("Reading all policy holders.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from PolicyHolder p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }
    
    public List<PolicyHolder> readAllPolicyHoldersCriteria(EntityManagerFactory emf) {
        System.out.println("Reading all policy holders using criteria.");
        
        EntityManager em = emf.createEntityManager();
        
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(PolicyHolder.class);
        Query query = em.createQuery(criteria);
        List<PolicyHolder> result = query.getResultList();
            
        displayResult(result);
        em.close();
        return result;
    }
    
    public PolicyHolder findPolicyHolder(EntityManagerFactory emf) {
        System.out.println("Find policy holder by Id.");
        
        EntityManager em = emf.createEntityManager();
        
        // TODO: Does not work.
        //Long id = (Long)em.createQuery("Select Min(p.id) from PolicyHolder p").getSingleResult();
        //PolicyHolder result = em.find(PolicyHolder.class, id);
        Long id = 1L;
        
        PolicyHolder result = em.find(PolicyHolderImpl.class, id);
            
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllPolicyHoldersJoinFetch(EntityManagerFactory emf) {
        System.out.println("Reading all policy holders joining policies and claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from PolicyHolder p join fetch p.claims join fetch p.policies").getResultList();
        for (PolicyHolder holder : result) {
            holder.getClaims().size();
            holder.getPolicies().size();
        }
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllPolicyHoldersBatch(EntityManagerFactory emf) {
        System.out.println("Reading all policy holders batching policies and claims.");
        
        EntityManager em = emf.createEntityManager();
        Query query = em.createQuery("Select p from PolicyHolder p");
        query.setHint(QueryHints.BATCH, "p.claims");
        query.setHint(QueryHints.BATCH, "p.policies");
        query.setHint(QueryHints.BATCH, "p.policies.claims");
        List<PolicyHolder> result = query.getResultList();
        for (PolicyHolder holder : result) {
            holder.getClaims().size();
            for (Policy policy : holder.getPolicies()) {
                policy.getClaims().size();
            }
        }
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllPersonalHoldersJoinFetch(EntityManagerFactory emf) {
        System.out.println("Reading all personal holders joining policies and claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select Distinct(p) from PersonalHolder p join fetch p.claims join fetch p.policies").getResultList();
        for (PolicyHolder holder : result) {
            holder.getClaims().size();
            holder.getPolicies().size();
        }
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllPolicies(EntityManagerFactory emf) {
        System.out.println("Reading all policies.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from Policy p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readPolicyJoinHolder(EntityManagerFactory emf) {
        System.out.println("Reading policy and holder.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from Policy p join fetch p.holder where p.holder.name='Bob Smith'").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllHealthPolicies(EntityManagerFactory emf) {
        System.out.println("Reading all health policies.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from HealthPolicy p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllVehiclePolicies(EntityManagerFactory emf) {
        System.out.println("Reading all vehicle policies.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from VehiclePolicy p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllMotorcyclePolicies(EntityManagerFactory emf) {
        System.out.println("Reading all motorcycle policies.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select p from MotorcyclePolicy p").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllClaims(EntityManagerFactory emf) {
        System.out.println("Reading all claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select c from Claim c").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllHealthClaims(EntityManagerFactory emf) {
        System.out.println("Reading all health claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select c from HealthClaim c").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllVehicleClaims(EntityManagerFactory emf) {
        System.out.println("Reading all vehicle claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select c from VehicleClaim c").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readAllMotorcycleClaims(EntityManagerFactory emf) {
        System.out.println("Reading all motorcycle claims.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select c from MotorcycleClaim c").getResultList();
        displayResult(result);
        em.close();
        return result;
    }

    public List<PolicyHolder> readClaimJoinHolder(EntityManagerFactory emf) {
        System.out.println("Reading claim and holder.");
        
        EntityManager em = emf.createEntityManager();
        List<PolicyHolder> result = em.createQuery("Select c from Claim c join fetch c.claimant where c.claimant.name='Bob Smith'").getResultList();
        displayResult(result);
        em.close();
        return result;
    }
    
    public void updateAllPolicyHolders(EntityManagerFactory emf) {
        // TODO: This should update all subclasses, but does not.
        System.out.println("Updating all policy holders.");
        
        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        em.createQuery("Update PolicyHolder p set p.version=p.version+1").executeUpdate();
        em.getTransaction().commit();
        em.close();
    }
    
    public void updateAllPolicies(EntityManagerFactory emf) {
        System.out.println("Updating all policies.");
        
        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        em.createQuery("Update Policy p set p.version=p.version+1").executeUpdate();
        em.getTransaction().commit();
        em.close();
    }
    
    public void updateAllClaims(EntityManagerFactory emf) {
        System.out.println("Updating all claims.");
        
        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        em.createQuery("Update Claim c set c.version=c.version+1").executeUpdate();
        em.getTransaction().commit();
        em.close();
    }
    
    public void clear(EntityManagerFactory factory) {
        System.out.println("Clearing cache.");
        factory.getCache().evictAll();        
    }
    
    public void displayResult(Object results) {
        System.out.println("Results:");
        if (results instanceof Collection) {
            for (Object result : (Collection)results) {
                System.out.println(result);
            }
        } else {
            System.out.println(results);            
        }
    }

}
