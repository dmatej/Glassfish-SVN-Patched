package eclipselink.example.jpa.employee.web.converters;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import eclipselink.example.jpa.employee.model.Company;

public class CompanyConverter implements Converter {
    
    @Override
    public Object getAsObject(FacesContext arg0, UIComponent arg1, String arg2) {
        return null;
    }

    @Override
    public String getAsString(FacesContext arg0, UIComponent arg1, Object arg2) {
        Company company = (Company) arg2;
        return company.getCode() + " - " + company.getName();
    }

}
