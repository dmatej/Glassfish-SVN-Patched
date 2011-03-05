package example;

import java.util.List;

import model.Employee;

public interface Example {

    public abstract List<Employee> findEmployees(String firstName, String lastName);

}
