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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import model.*;

/**
 * Simple query examples.
 */
public class Queries {

    public static int REPEATS = 5;
    public static int RUN_TIME = 60000; //1 minute.

    /**
     * Measure the performance of the run.
     * Repeat the run REPEATS (5) times,
     * and measure the number of execution in RUN_TIME (60s).
     */
    public static void executeRun(String name, Runnable runnable) {
        System.out.println("Starting run: " + name);
        
        List<Integer> results = new ArrayList<Integer>();
        // Repeat the test and baseline for the number of repeats.
        for (int index = 0; index < REPEATS; index++) {
            long startTime, endTime;
            int executions = 0;
            System.gc();
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ignore) {}
            startTime = System.currentTimeMillis();
            endTime = startTime;
            // Count how many times the test can be invoked in the run time.
            // This allows for the test run time to be easily changed.
            while ((startTime + RUN_TIME) >= endTime) {
                runnable.run();
                executions++;
                endTime = System.currentTimeMillis();
            }
            results.add(executions);
            System.out.println("Done run: " +  index + " for: " + name);
        }
        
        System.out.println("Completed run: " + name);

        System.out.println("");
        System.out.println(name + " Results");
        System.out.println("Run repeats:" + REPEATS);
        System.out.println("Run time:" + RUN_TIME + "ms");
        System.out.println("Average result:" + averageResults(results));
        System.out.println("Max result:" + maxResults(results));
        System.out.println("Min result:" + minResults(results));
        System.out.println("% standard deviation:" + standardDeviationResults(results));
        System.out.println("");
        System.out.println("");        
    }

    /**
     * Compute the max of the results.
     */
    public static int maxResults(List<Integer> times) {
        int testMax = 0;
        for (int index = 0; index < times.size(); index++) {
            int time = (int)times.get(index);
            if (time > testMax) {
                testMax = time;
            }
        }
        return testMax;
    }

    /**
     * Compute the min of the results.
     */
    public static int minResults(List<Integer> times) {
        int testMin = 0;
        for (int index = 0; index < times.size(); index++) {
            int time = (int)times.get(index);
            if ((testMin == 0) || (time < testMin)) {
                testMin = time;
            }
        }
        return testMin;
    }

    /**
     * Filter max and min from results.
     */
    public static List<Integer> filterMaxMinResults(List<Integer> times) {
        List filteredTimes = new ArrayList(times);
        if (filteredTimes.size() > 3) {
            filteredTimes.remove((Integer)maxResults(times));
            filteredTimes.remove((Integer)minResults(times));
        }
        return filteredTimes;
    }

    /**
     * Compute the average of the results rejecting the min and max.
     */
    public static double averageResults(List<Integer> allTimes) {
        // Compute the average reject the min and max to improve consistency.
        List<Integer> times = filterMaxMinResults(allTimes);
        double testAverage = 0;
        for (int index = 0; index < times.size(); index++) {
            int time = (int)times.get(index);
            testAverage = testAverage + time;
        }
        testAverage = testAverage / times.size();
        return testAverage;
    }

    /**
     * Compute the standard deviation of the results rejecting the min and max.
     */
    public static double standardDeviationResults(List<Integer> allTimes) {
        // Compute the average reject the min and max to improve consistency.
        double testAverage = averageResults(allTimes);

        // Compute the standard deviation reject the min and max to improve consistency.
        List<Integer> times = filterMaxMinResults(allTimes);
        double testStandardDeviation = 0;
        for (int index = 0; index < times.size(); index++) {
            int time = (int)times.get(index);
            testStandardDeviation = testStandardDeviation + Math.pow(time - testAverage, 2);
        }
        testStandardDeviation = testStandardDeviation / times.size();
        testStandardDeviation = Math.sqrt(testStandardDeviation);
        // As percent of average
        testStandardDeviation = (testStandardDeviation / testAverage) * 100;
        return testStandardDeviation;
    }


    public static void main(String[] args) throws Exception {        
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");
        //for (int index = 0; index < 5000; index++) {
            Populate.populate(emf);
        //}
        Queries queries = new Queries();
        queries.clear(emf);
        queries.runSimpleQuery("standardFindAllBySalary", emf);
        queries.runSimpleQuery("simpleJoinFetchFindAllBySalary", emf);
        queries.runSimpleQuery("simpleBatchJOINFetchFindAllBySalary", emf);
        queries.runSimpleQuery("simpleBatchEXISTSFetchFindAllBySalary", emf);
        queries.runSimpleQuery("simpleBatchINFetchFindAllBySalary", emf);
        
        queries.runComplexQuery("standardFindAllBySalary", emf);
        queries.runComplexQuery("joinFetchFindAllBySalary", emf);
        queries.runComplexQuery("batchJOINFetchFindAllBySalary", emf);
        queries.runComplexQuery("batchEXISTSFetchFindAllBySalary", emf);
        queries.runComplexQuery("batchINFetchFindAllBySalary", emf);
        emf.close();
    }

    public void runSimpleQuery(final String queryName, final EntityManagerFactory emf) {
        
        executeRun(queryName, new Runnable() {
            public void run() {
                EntityManager em = emf.createEntityManager();
                Query query = em.createNamedQuery(queryName);
                query.setParameter("salary", 50000);
                List<Employee> result = query.getResultList();
                for (Employee employee : result) {
                    // Ensure every relationship is accessed.
                    String.valueOf(employee.getAddress());
                    employee.getPhoneNumbers().size();
                }
                em.close();
                clear(emf);
            }
        });
    }
    
    public void runComplexQuery(final String queryName, final EntityManagerFactory emf) {
        
        executeRun(queryName, new Runnable() {
            public void run() {
                EntityManager em = emf.createEntityManager();
                Query query = em.createNamedQuery(queryName);
                query.setParameter("salary", 50000);
                List<Employee> result = query.getResultList();
                for (Employee employee : result) {
                    // Ensure every relationship is accessed.
                    String.valueOf(employee.getAddress());
                    employee.getManagedEmployees().size();
                    employee.getDegrees().size();
                    String.valueOf(employee.getManager());
                    String.valueOf(employee.getJobTitle());
                    employee.getEmailAddresses().size();
                    employee.getPhoneNumbers().size();
                    employee.getProjects().size();
                    employee.getResponsibilities().size();
                }
                em.close();
                clear(emf);
            }
        });
    }
    
    public void clear(EntityManagerFactory factory) {
        factory.getCache().evictAll();        
    }
}
