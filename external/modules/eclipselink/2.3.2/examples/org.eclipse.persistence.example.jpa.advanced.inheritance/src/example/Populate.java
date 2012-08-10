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

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import model.*;

/**
 * This example populates the database with example instances using JPA.
 */
public class Populate {

    public static void main(String[] args) {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");
        populate(emf);
        emf.close();
    }

    public static void populate(EntityManagerFactory emf) {
        EntityManager em = emf.createEntityManager();
        // TODO: Remove this when the table-per-class table creation issue is fixed.
        em.getTransaction().begin();
        em.createNativeQuery("ALTER TABLE CLAIM DROP CONSTRAINT FK_CLAIM_CLAIMANT_ID").executeUpdate();
        em.createNativeQuery("ALTER TABLE POLICY DROP CONSTRAINT FK_POLICY_HOLDER_ID").executeUpdate();        
        em.getTransaction().commit();
        em.getTransaction().begin();
        Populate.population.persistAll(em);
        em.getTransaction().commit();
        em.close();
    }

    public static final Populate population = new Populate();

    private Populate() {

    }

    /**
     * Register all of the population in the provided EntityManager to be
     * persisted This method should only be called from within a test case. It
     * asserts that the provided EntityManager is in a transaction and that the
     * database tables are empty.
     */
    public void persistAll(EntityManager em) {
        System.out.println("Persisting samples objects.");
        
        em.persist(holder1());
        em.persist(holder2());
        em.persist(holder3());

        System.out.println("Flushing to database.");
        em.flush();
    }

    public PolicyHolder holder1() {
        PolicyHolder holder = new PersonalHolder();
        holder.setName("Bob Smith");

        HealthPolicy policy = new HealthPolicy();
        policy.setCoverage(1000000);
        policy.setDescription("Personal health insurance.");
        
        holder.addPolicy(policy);
        
        HealthClaim claim = new HealthClaim();
        claim.setAmount(1000);
        claim.setCondition("Broken bone");
        claim.setDescription("Broken Arm");
        claim.setDoctor("Dr. Jon Smyth");
        
        policy.addClaim(claim);
        holder.addClaim(claim);

        VehiclePolicy vehiclePolicy = new AutoPolicy();
        vehiclePolicy.setCoverage(1000000);
        vehiclePolicy.setVehicleMake("BMW");
        vehiclePolicy.setVehicleModel("BMW 420");
        vehiclePolicy.setDescription("Personal auto insurance.");
        
        holder.addPolicy(vehiclePolicy);
        
        AutoClaim autoClaim = new AutoClaim();
        autoClaim.setAmount(60000);
        autoClaim.setDescription("Collision");
        autoClaim.setVehicleMake("BMW");
        autoClaim.setVehicleModel("BMW 420");
        
        policy.addClaim(autoClaim);
        holder.addClaim(autoClaim);
        
        autoClaim = new AutoClaim();
        autoClaim.setAmount(10000);
        autoClaim.setDescription("Collision");
        autoClaim.setVehicleMake("Ford");
        autoClaim.setVehicleModel("Ford 350");
        
        policy.addClaim(autoClaim);
        holder.addClaim(autoClaim);
        
        return holder;
    }

    public PolicyHolder holder2() {
        PolicyHolder holder = new PersonalHolder();
        holder.setName("Jane Doe");

        HealthPolicy policy = new HealthPolicy();
        policy.setCoverage(5000000);
        policy.setDescription("Personal health insurance.");
        
        holder.addPolicy(policy);
        
        HealthClaim claim = new HealthClaim();
        claim.setAmount(50);
        claim.setCondition("Checkup");
        claim.setDescription("Anual checkup");
        claim.setDoctor("Dr. Jane Way");
        
        policy.addClaim(claim);
        holder.addClaim(claim);
        
        claim = new HealthClaim();
        claim.setAmount(500);
        claim.setCondition("Vaccine");
        claim.setDescription("Flue shot");
        claim.setDoctor("Dr. Jane Way");
        
        policy.addClaim(claim);
        holder.addClaim(claim);
        
        return holder;
    }

    public PolicyHolder holder3() {
        CorporateHolder holder = new CorporateHolder();
        holder.setName("ACME Corp.");

        HealthPolicy policy = new HealthPolicy();
        policy.setCoverage(50000000);
        policy.setDescription("Corporate health insurance.");
        
        holder.addPolicy(policy);

        CorporatePolicy corpPolicy = new CorporatePolicy();
        corpPolicy.setCoverage(100000000);
        corpPolicy.setDescription("Corporate policy.");
        
        holder.setPolicy(corpPolicy);
        
        return holder;
    }
}
