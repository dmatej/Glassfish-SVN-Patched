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

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.IAdapterManager;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Issue;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Publisher;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Title;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.DeferredTreeContentManager;

public class View extends ViewPart {
    public static final String ID = "org.eclipse.persistence.example.jpa.rcp.comics.view";

    private Model model;
    private IAdapterFactory modelFactory = new ModelAdapterFactory();
    private IAdapterFactory publisherFactory = new PublisherAdapterFactory();
    private IAdapterFactory titleFactory = new TitleAdapterFactory();
    private IAdapterFactory issueFactory = new IssueAdapterFactory();

    private TreeViewer viewer;

    class ViewContentProvider extends BaseWorkbenchContentProvider {
        private DeferredTreeContentManager manager;
        
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            manager = new DeferredTreeContentManager(this, (AbstractTreeViewer) viewer);
        }

        public Object[] getChildren(Object parentElement) {
            return manager.getChildren(parentElement);
        }

        public boolean hasChildren(Object element) {
            return manager.mayHaveChildren(element);
        }
    }

    public void createPartControl(Composite parent) {
        initializeModel();

        IAdapterManager adapterManager = Platform.getAdapterManager();
        adapterManager.registerAdapters(modelFactory, Model.class);
        adapterManager.registerAdapters(publisherFactory, Publisher.class);
        adapterManager.registerAdapters(titleFactory, Title.class);
        adapterManager.registerAdapters(issueFactory, Issue.class);

        viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        viewer.setContentProvider(new ViewContentProvider());
        viewer.setLabelProvider(new WorkbenchLabelProvider());

        viewer.setInput(model.getRoot());
    }

    public void initializeModel() {
        model = new Model();
    }


    public void setFocus() {
        viewer.getControl().setFocus();
    }
    
    @Override
    public void dispose() {
        IAdapterManager adapterManager = Platform.getAdapterManager();
        adapterManager.unregisterAdapters(modelFactory);    
        adapterManager.unregisterAdapters(publisherFactory);
        adapterManager.unregisterAdapters(titleFactory);
        adapterManager.unregisterAdapters(issueFactory);
        model.dispose();
        super.dispose();
    }

}