package eclipselink.example.jpa.employee.web.beans.admin;

import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.web.beans.BaseManagedBean;
import eclipselink.example.jpa.employee.web.beans.EmployeeService;

public class EditUserBean extends BaseManagedBean {

    private User user = new User();

    private EmployeeService service;

    public EmployeeService getService() {
        return service;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public String setUserToEdit(String id) {
        return null;
    }

    public String create() {
        this.user = new User();
        return "create-user";
    }

    public String save() {

        System.out.println("EditUserBean.save:");
        System.out.println("\tUser id: " + getUser().getId());
        // TODO
        return null;
    }

}
