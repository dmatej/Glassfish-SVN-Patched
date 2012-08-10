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
 *      Oracle - initial impl
 ******************************************************************************/
package example;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import model.*;

/**
 * Simulates a large load of order processing.
 * @author James Sutherland
 */
public class Test {

    public static int REPEATS = 5;
    public static int THREADS = 16;
    public static int RUN_TIME = 600;//60000; //1 minute.

    protected static volatile int executions;
    
    /**
     * Measure the performance of the run.
     * Repeat the run REPEATS (5) times,
     * and measure the number of execution in RUN_TIME (60s).
     */
    public static void executeRun(String name, final Runnable runnable) {
        System.out.println("Starting run: " + name);
        
        List<Integer> results = new ArrayList<Integer>();
        // Repeat the test and baseline for the number of repeats.
        for (int repeat = 0; repeat < REPEATS; repeat++) {
            System.gc();
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ignore) {}
            executions = 0;
            Thread[] threads = new Thread[THREADS];
            for (int thread = 0; thread < THREADS; thread++) {
                Runnable worker = new Runnable() {
                    public void run() {
                        long startTime, endTime;
                        startTime = System.currentTimeMillis();
                        endTime = startTime;
                        // Count how many times the test can be invoked in the run time.
                        // This allows for the test run time to be easily changed.
                        while ((startTime + RUN_TIME) >= endTime) {
                            runnable.run();
                            executions++;
                            endTime = System.currentTimeMillis();
                        }
                    }
                };
                threads[thread] = new Thread(worker);                
            }
            for (Thread thread : threads) {
                thread.start();
            }
            for (Thread thread : threads) {
                try {
                    thread.join();
                } catch (InterruptedException ignore) {}
            }
            results.add(executions);
            System.out.println("Done run: " +  repeat + " for: " + name);
        }
        
        System.out.println("Completed run: " + name);

        System.out.println("");
        System.out.println(name + " Results");
        System.out.println("Run repeats:" + REPEATS);
        System.out.println("Run threads:" + THREADS);
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
            int time = times.get(index);
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
            int time = times.get(index);
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
            int time = times.get(index);
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
            int time = times.get(index);
            testStandardDeviation = testStandardDeviation + Math.pow(time - testAverage, 2);
        }
        testStandardDeviation = testStandardDeviation / times.size();
        testStandardDeviation = Math.sqrt(testStandardDeviation);
        // As percent of average
        testStandardDeviation = (testStandardDeviation / testAverage) * 100;
        return testStandardDeviation;
    }


    public static void main(String[] args) throws Exception {
        final EntityManagerFactory emf = Persistence.createEntityManagerFactory("order");
        EntityManager em = emf.createEntityManager();
        // Drop constraint to allow cross partition relationship.
        em.getTransaction().begin();
        em.createNativeQuery("ALTER TABLE PART_ORDER DROP FOREIGN KEY FK_PART_ORDER_CUSTOMER_ID").executeUpdate();
        em.getTransaction().commit();
        em.close();
        executeRun("place-orders", new Runnable() {
            ThreadLocal<Customer> customer = new ThreadLocal<Customer>();
            public void run() {
                Test test = new Test();
                if (customer.get() == null) {
                    customer.set(test.createCustomer(emf));
                }
                test.placeOrder(customer.get(), emf);
            }
        });
        emf.close();
    }

    public Customer createCustomer(EntityManagerFactory emf) {        
        EntityManager em = emf.createEntityManager();
        Customer customer = new Customer();
        customer.setName("AMCE-" + System.currentTimeMillis());
        em.getTransaction().begin();
        em.persist(customer);
        em.getTransaction().commit();
        em.close();
        return customer;
    }

    public Order placeOrder(Customer customer, EntityManagerFactory emf) {        
        EntityManager em = emf.createEntityManager();
        customer = em.find(customer.getClass(), customer.getId());
        Order order = new Order();
        order.setDescription("Order-" + System.currentTimeMillis());
        for (int index = 0; index<10; index++) {
            order.addOrderLine(new OrderLine("item-" + index, new BigDecimal(index * 100)));
        }
        customer.addOrder(order);
        em.getTransaction().begin();
        em.persist(order);
        em.getTransaction().commit();
        em.close();
        return order;
    }
}
