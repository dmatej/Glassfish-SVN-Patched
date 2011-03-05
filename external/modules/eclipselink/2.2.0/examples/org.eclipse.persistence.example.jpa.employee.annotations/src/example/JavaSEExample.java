package example;

import javax.persistence.*;

import model.Employee;

public class JavaSEExample {

    public static void main(String[] args) {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee-annotations");

        EntityManager em = emf.createEntityManager();

        System.out.println("\n\n --- Query for all Employee ---");
        em.createQuery("SELECT e FROM Employee e").getResultList();

        System.out.println("\n\n --- Create New Employee ---");
        em.getTransaction().begin();
        
        Employee newEmp = new Employee();
        newEmp.setFirstName("Doug");
        newEmp.setLastName("Clarke");
        
        em.persist(newEmp);
        
        em.getTransaction().commit();
        
        
        
        System.out.println("\n\n --- Modify Employee ---");
        em.getTransaction().begin();
        
        Employee emp = em.find(Employee.class, newEmp.getId());
        emp.setSalary(1);
        
        em.getTransaction().commit();
        
        
        
        System.out.println("\n\n --- Delete Employee ---");
        em.getTransaction().begin();
        
        em.remove(em.find(Employee.class, newEmp.getId()));
        
        em.getTransaction().commit();
        
        
        
        em.close();
        emf.close();
    }
}
