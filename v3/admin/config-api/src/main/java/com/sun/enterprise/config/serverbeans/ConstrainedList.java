package com.sun.enterprise.config.serverbeans;

import java.beans.PropertyChangeEvent;
import java.beans.VetoableChangeSupport;
import java.beans.PropertyVetoException;
import java.util.ArrayList;

/**
 * A Constrained List is a @Link java.util.List implementation which mutable
 * operations are constrained by the owner of the list.
 *
 * @author Jerome Dochez
 */
public class ConstrainedList<T> extends ArrayList<T> {

    final String id;

    final VetoableChangeSupport support;

    ConstrainedList(String id, VetoableChangeSupport support) {
        this.id = id;
        this.support = support;
    }

    public boolean add(T object) {
        try {
            support.fireVetoableChange(new PropertyChangeEvent(this, id, null, object));
        } catch (PropertyVetoException e) {
            return false;
        }
        return super.add(object);
    }
}
