/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle., Eclipse Foundation All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     tware, ssmith - 1.0 - RCP Demo
 ******************************************************************************/  
package org.eclipse.persistence.example.jpa.rcp.comics;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.progress.IDeferredWorkbenchAdapter;
import org.eclipse.ui.progress.IElementCollector;

public abstract class AbstractAdapter implements IDeferredWorkbenchAdapter {

    public AbstractAdapter() {
        super();
    }

    public abstract Object[] getChildren(Object o);

    public void fetchDeferredChildren(Object object, IElementCollector collector, IProgressMonitor monitor) {
        Object[] children = getChildren(object);
        monitor.beginTask("Loading contents", children.length);
        for (int i = 0; i < children.length; i++) {
            if (monitor.isCanceled()) break;
            collector.add(children[i], monitor);
            monitor.worked(1);
        }
        monitor.done();
        collector.done();
    }

    public ISchedulingRule getRule(Object arg0) {
        // TODO: Ensure that we don't run concurrent queries
        // return JPASchedulingRule.getInstance();
        return null;
    }

    public boolean isContainer() {
        return true;
    }

    public ImageDescriptor getImageDescriptor(Object object) {
        return null;
    }

    public Object getParent(Object o) {
        return null;
    }

}