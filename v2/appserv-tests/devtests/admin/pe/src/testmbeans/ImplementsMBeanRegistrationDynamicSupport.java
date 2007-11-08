//GEN-BEGIN:generatedCode
/**
 * ImplementsMBeanRegistrationDynamicSupport.java
 *
 * @author kedarm
 * Created on March 17, 2006, 5:59 PM
 */
package testmbeans;
import javax.management.*;
import java.lang.reflect.Constructor;
import java.util.Iterator;

/**
 * Generic Dynamic MBean treatment.
 * This class implements <CODE>DynamicMBean</CODE> interface.
 * It provides a generic implementation of <CODE>getAttributes</CODE> and
 * <CODE>setAttributes</CODE>.
 * For each attribute present in the received list, a call to abstract
 * methods <CODE>getAttribute</CODE> or <CODE>setAttribute</CODE> is performed.
 * <p>
 * Extended Classes must implement <CODE>getAttribute</CODE> and
 * <CODE>setAttribute</CODE> methods.
 */
public abstract class ImplementsMBeanRegistrationDynamicSupport implements javax.management.DynamicMBean
{
    /* Creates a new instance of ImplementsMBeanRegistrationDynamicSupport */
    public ImplementsMBeanRegistrationDynamicSupport() {
    }
    
    /**
     * Gets the value of the specified attribute of the DynamicMBean.
     * @param attributeName The attribute name
     */
    public abstract Object getAttribute(String attributeName)
    throws AttributeNotFoundException,
            MBeanException,
            ReflectionException;
    
    /**
     * Sets the value of the specified attribute of the DynamicMBean.
     * @param attribute The attribute to set
     */
    public abstract void setAttribute(Attribute attribute)
    throws AttributeNotFoundException,
            InvalidAttributeValueException,
            MBeanException,
            ReflectionException;
    
    /**
     * Create the MBeanInfoConstructors.
     */
    public static MBeanConstructorInfo[] createConstructors(Class clzz) {
        Constructor[] constructors = clzz.getConstructors();
        MBeanConstructorInfo[] infos = new MBeanConstructorInfo[
                constructors.length];
        for (int i = 0; i < constructors.length; i++) {
            infos[i] = new MBeanConstructorInfo("Constructs a"
                    + clzz.getName() + "object", constructors[i]);
        }
        return infos;
    }
    
    /**
     * Enables the get values of several attributes of the Dynamic MBean.
     * @param attributeNames Array of attribute names
     */
    public AttributeList getAttributes(String[] attributeNames) {
        AttributeList resultList = new AttributeList();
        // build the result attribute list
        for (int i = 0 ; i < attributeNames.length ; i++) {
            try {
                Object value = getAttribute((String) attributeNames[i]);
                resultList.add(new Attribute(attributeNames[i],value));
            } catch (Exception e) {//GEN-END:generatedCode
                //e.printStackTrace();
            }//GEN-BEGIN:generatedCode1
        }
        return resultList;
    }
    
    /**
     * Sets the values of several attributes of the Dynamic MBean
     * and returns the list of attributes that have been set.
     * @param attributes The list of attributes to set
     * @return List of set Attributes    */
    public AttributeList setAttributes(AttributeList attributes) {
        AttributeList resultList = new AttributeList();
        // For each attribute, try to set it and add to the result list
        // if succesfull
        for (Iterator i = attributes.iterator(); i.hasNext();) {
            Attribute attr = (Attribute) i.next();
            try {
                setAttribute(attr);
                String name = attr.getName();
                Object value = getAttribute(name);
                resultList.add(new Attribute(name,value));
            } catch (Exception e) {//GEN-END:generatedCode1
                //e.printStackTrace();
            }//GEN-BEGIN:generatedCode2
        }
        return resultList;
    }
}
//GEN-END:generatedCode2
