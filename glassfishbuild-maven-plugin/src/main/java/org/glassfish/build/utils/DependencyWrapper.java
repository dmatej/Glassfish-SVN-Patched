/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.glassfish.build.utils;

import org.apache.maven.model.Dependency;

/**
 *
 * @author romano
 */
public class DependencyWrapper {
    private Dependency d;
    private String stringRepresentation;
    private int hashCode;

    public DependencyWrapper(Dependency d) {
        this.d = d;
        this.stringRepresentation = String.valueOf(d);
        this.hashCode = stringRepresentation.hashCode();
    }

    public Dependency getDependency() {
        return d;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final DependencyWrapper other = (DependencyWrapper) obj;
        return this.stringRepresentation.equals(other.stringRepresentation);
    }

    @Override
    public int hashCode() {
        return hashCode;
    }
}
