package com.sun.enterprise.config.serverbeans;

import java.beans.VetoableChangeSupport;
import java.beans.VetoableChangeListener;

/**
 * ConstrainedBean interface is implemented by config beans that supports
 * JavaBeans constrained properties
 *
 * @author Jerome Dochez
 */
public interface ConstrainedBean {

    public VetoableChangeSupport getVetoableChangeSupport();

    public void addVetoableChangeListener(VetoableChangeListener param0);

    public void addVetoableChangeListener(String param0, VetoableChangeListener param1);

    public void removeVetoableChangeListener(String param0, VetoableChangeListener param1);

    public void removeVetoableChangeListener(VetoableChangeListener param0);

}
