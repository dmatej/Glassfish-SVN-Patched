/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.component;

import java.io.IOException;

import java.util.AbstractMap;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;

import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;

import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.PhaseId;

import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.LocalRowKeyIndex;
import org.apache.myfaces.trinidad.model.ModelUtils;

import org.apache.myfaces.trinidad.render.ClientRowKeyManager;

/**
 * This component iterates over some given data.
 * Each child is repeatedly stamped as many times as necessary.
 * Iteration is done starting at the index given by {@link #getFirst()}
 * for as many indices as specified by {@link #getRows()}.
 * If {@link #getRows()} returns 0, then the iteration continues until
 * there are no more elements in the underlying data.
 */
public abstract class UIXIteratorTemplate extends UIXCollection implements FlattenedComponent, LocalRowKeyIndex
{

/**/  abstract public int getFirst();
/**/  abstract public void setFirst(int first);
/**/  abstract public int getRows();

  /**
   * Override to return true.
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Sets up the iteration context for each child and processes it
   */
  public <S> boolean processFlattenedChildren(
    final FacesContext context,
    ComponentProcessingContext cpContext,
    final ComponentProcessor<S> childProcessor,
    final S callbackContext) throws IOException
  {
    boolean processedChildren;

    setupVisitingContext(context);

    try
    {
      // Mimic what would normally happen in the non-flattening case for encodeBegin():
      __processFlattenedChildrenBegin();

      setupChildrenVisitingContext(context);

      try
      {
        Runner runner = new IndexedRunner(cpContext)
        {
          @Override
          protected void process(UIComponent kid, ComponentProcessingContext cpContext) throws IOException
          {
            kid.pushComponentToEL(context, null);

            try
            {
              childProcessor.processComponent(context, cpContext, kid, callbackContext);
            }
            finally
            {
              kid.popComponentFromEL(context);
            }
          }
        };

        processedChildren = runner.run();
        Exception exp = runner.getException();
        if (exp != null)
        {
          if (exp instanceof RuntimeException)
            throw (RuntimeException) exp;

          if (exp instanceof IOException)
            throw (IOException) exp;
          throw new IllegalStateException(exp);
        }
      }
      finally
      {
        tearDownChildrenVisitingContext(context);
      }
    }
    finally
    {
      tearDownVisitingContext(context);
    }

    return processedChildren;
  }

  /**
   * Returns <code>true</code> if this FlattenedComponent is currently flattening its children
   * @param context FacesContext
   * @return <code>true</code> if this FlattenedComponent is currently flattening its children
   */
  public boolean isFlatteningChildren(FacesContext context)
  {
    // if we don't have a Renderer, then we're flattening
    return (getRendererType() == null);
  }

  /**
   * Repeatedly render the children as many times as needed.
   */
  @Override
  public void encodeChildren(final FacesContext context)
    throws IOException
  {
    if (!isRendered())
      return;

    // if this is the table there will be a rendererType:
    if (getRendererType() != null)
    {
      Renderer renderer = getRenderer(context);
      if (renderer != null)
      {
        renderer.encodeChildren(context, this);
      }
    }
    else // this is not the table. it must be the iterator
    {
      Runner runner = new IndexedRunner()
      {
        @Override
        protected void process(
          UIComponent                kid,
          ComponentProcessingContext cpContext
          ) throws IOException
        {
          kid.encodeAll(context);
        }
      };
      runner.run();
      Exception exp = runner.getException();
      if (exp != null)
      {
        if (exp instanceof RuntimeException)
          throw (RuntimeException) exp;

        if (exp instanceof IOException)
          throw (IOException) exp;
        throw new IllegalStateException(exp);
      }
    }
  }

  /**
   * Enhances the varStatusMap created by the super class to include:<ul>
   * <li>begin - the index of the first row being rendered
   * <li>first - true if the current row is the first row
   * <li>count - indicates which iteration this is. This always starts at one,
   * and increases (by one) as the loop progresses.
   * <li>step - this is always one.
   * </ul>
   */
  @Override
  protected Map<String, Object> createVarStatusMap()
  {
    final Map<String, Object> map = super.createVarStatusMap();
    return new AbstractMap<String, Object>()
    {
      @Override
      public Object get(Object key)
      {
        // some of these keys are from <c:forEach>, ie:
        // javax.servlet.jsp.jstl.core.LoopTagStatus
        if ("begin".equals(key)) // from jstl
        {
          return Integer.valueOf(getFirst());
        }
        if ("first".equals(key)) // from jstl
        {
          boolean isFirst = (getFirst() == getRowIndex());
          return Boolean.valueOf(isFirst);
        }
        if ("count".equals(key)) // from jstl
        {
          int count = getRowIndex() - getFirst() + 1;
          return Integer.valueOf(count);
        }
        if ("step".equals(key)) // from jstl
        {
          return Integer.valueOf(1);
        }
        return map.get(key);
      }

      @Override
      public Set<Map.Entry<String, Object>> entrySet()
      {
        return map.entrySet();
      }
    };
  }

  @Override
  protected CollectionModel createCollectionModel(
    CollectionModel current,
    Object value)
  {
    CollectionModel model = ModelUtils.toCollectionModel(value);
    // initialize to -1. we need to do this incase some application logic
    // changed this index. Also, some JSF1.0 RI classes were initially starting
    // with a rowIndex of 0.
    // we need this to be -1 because of name-transformation.
    model.setRowIndex(-1);
    assert model.getRowIndex() == -1 : "RowIndex did not reset to -1";
    return model;
  }

  @Override
  protected void processFacetsAndChildren(
    final FacesContext context,
    final PhaseId phaseId)
  {
    Runner runner = new IndexedRunner()
    {
      @Override
      protected void process(UIComponent kid, ComponentProcessingContext cpContext)
      {
        UIXIterator.this.processComponent(context, kid, phaseId);
      }
    };
    runner.run();
  }

  // Extract the current row token from the clientId
  private String _getClientToken(String clientIdPrefix, String cellClientId)
  {
    int tokenStartIndex = clientIdPrefix.length() + 1;
    int tokenEndIndex = cellClientId.indexOf(':', tokenStartIndex);

    if (tokenEndIndex != -1)
    {
      return cellClientId.substring(tokenStartIndex, tokenEndIndex);
    }
    else
    {
      return null;
    }
  }

  @Override
  protected boolean visitData(
    final VisitContext  visitContext,
    final VisitCallback visitCallback)
  {
    Collection<String> subtreeIds = visitContext.getSubtreeIdsToVisit(this);

    // create a special VisitContext that doesn't visit the Facets
    // of column components since they aren't visited on each row
    final VisitContext noColumnFacetContext = new NoColumnFacetsVisitContext(visitContext);

    // runner to use to process the rows
    Runner runner;

    if (VisitContext.ALL_IDS.equals(subtreeIds))
    {
      // we're processing all of the rows, so use the indexed runner (plus, we can't call size() on
      // the ALL_IDS collection, so we don't have a whole lot of choice here
      runner = new IndexedRunner()
      {
        @Override
        protected void process(UIComponent kid, ComponentProcessingContext cpContext)
        {
          if (UIXComponent.visitTree(noColumnFacetContext, kid, visitCallback))
          {
            throw new AbortProcessingException();
          }
        }
      };
    }
    else
    {
      // We are only visiting a subset of the tree, so figure out which rows to visit

      String ourClientIdPrefix = getClientId(visitContext.getFacesContext());

      int subtreeIdCount = subtreeIds.size();

      // build up a set of the row keys to visit rather than iterating
      // and visiting every row
      Set<String> rowsToVisit;

      if (subtreeIdCount > 1)
      {
        rowsToVisit = new HashSet<String>(subtreeIdCount);

        for (String currClientId : subtreeIds)
        {
          String clientToken = _getClientToken(ourClientIdPrefix, currClientId);

          if (clientToken != null)
          {
            rowsToVisit.add(clientToken);
          }
        }
      }
      else
      {
        String clientToken = _getClientToken(ourClientIdPrefix,
                                             subtreeIds.iterator().next());

        if (clientToken != null)
        {
          rowsToVisit = Collections.singleton(clientToken);
        }
        else
        {
          rowsToVisit = Collections.emptySet();
        }
      }

      // we didn't visit any data
      if (rowsToVisit.isEmpty())
        return false;

      // visit only the rows we need to
      runner = new KeyedRunner(rowsToVisit)
      {
        @Override
        protected void process(
          UIComponent                kid,
          ComponentProcessingContext cpContext
          ) throws IOException
        {
          if (UIXComponent.visitTree(noColumnFacetContext, kid, visitCallback))
          {
            throw new AbortProcessingException();
          }
        }
      };
    }

    try
    {
      runner.run();
    }
    finally
    {
      return (runner.getException() instanceof AbortProcessingException);
    }
  }

  /**
   * Abstract class for processing rows
   */
  private abstract class Runner implements ComponentProcessor<Object>
  {
    public Runner()
    {
      this(null);
    }

    public Runner(ComponentProcessingContext cpContext)
    {
      _cpContext = cpContext;
    }

    public abstract boolean run();

    /**
     * Sets up the context for the child and processes it
     */
    public void processComponent(
      FacesContext context,
      ComponentProcessingContext cpContext,
      UIComponent component,
      Object callbackContext) throws IOException
    {
      try
      {
        process(component, cpContext);
      }
      catch (IOException ioe)
      {
        throw ioe;
      }
      catch (AbortProcessingException ape)
      {
        // we're done, so abort
        _exception = ape;
        throw ape;
      }
      catch (Exception e)
      {
        _exception = e;
      }
    }

    public Exception getException()
    {
      return _exception;
    }

    protected abstract void process(UIComponent comp, ComponentProcessingContext cpContext)
      throws Exception;

    protected final ComponentProcessingContext getComponentProcessingContext()
    {
      return _cpContext;
    }

    public final void setException(Exception e)
    {
      _exception = e;
    }

    private Exception _exception = null;

    private final ComponentProcessingContext _cpContext;
  }

  /**
   * Class for visiting getRows() by index rows starting getFirst()
   */
  private abstract class IndexedRunner extends Runner
  {
    public IndexedRunner()
    {
      this(null);
    }

    public IndexedRunner(ComponentProcessingContext cpContext)
    {
      super(cpContext);
    }

    public final boolean run()
    {
      FacesContext context = FacesContext.getCurrentInstance();
      ComponentProcessingContext cpContext = getComponentProcessingContext();

      List<UIComponent> stamps = getStamps();
      int oldIndex = getRowIndex();
      int first = getFirst();
      int rows = getRows();
      int end = (rows <= 0) //show everything
        ? Integer.MAX_VALUE
        : first + rows;

      boolean processedChild = false;

      try
      {
        for(int i=first; i<end; i++)
        {
          setRowIndex(i);
          if (isRowAvailable())
          {
            // latch processedChild the first time we process a child
            processedChild |= (cpContext != null)
              ? UIXComponent.processFlattenedChildren(context, cpContext, this, stamps, null)
              : UIXComponent.processFlattenedChildren(context, this, stamps, null);
          }
          else
            break;
        }
      }
      catch (IOException e)
      {
        setException(e);
      }
      finally
      {
        setRowIndex(oldIndex);
      }

      return processedChild;
    }
  }

  /**
   * Runner that visits the rows specified by the client row key tokens
   */
  private abstract class KeyedRunner extends Runner
  {
    public KeyedRunner(Iterable<String> clientKeys)
    {
      super();
      _clientKeys = clientKeys;
    }

    public final boolean run()
    {
      FacesContext context = FacesContext.getCurrentInstance();

      List<UIComponent> stamps = getStamps();
      int oldIndex = getRowIndex();

      boolean processedChild = false;

      try
      {
        // need to convert row key tokens to row keys
        ClientRowKeyManager rowKeyManager = getClientRowKeyManager();

        for(String clientKey : _clientKeys)
        {
          Object rowKey = rowKeyManager.getRowKey(context, UIXIterator.this, clientKey);

          if (rowKey != null)
          {
            setRowKey(rowKey);
            if (isRowAvailable())
            {
              // latch processedChild the first time we process a child
              processedChild |= UIXComponent.processFlattenedChildren(context, this, stamps, null);
            }
          }
        }
      }
      catch (IOException e)
      {
        setException(e);
      }
      finally
      {
        setRowIndex(oldIndex);
      }

      return processedChild;
    }

    private final Iterable<String> _clientKeys;
  }

  @Override
  void __encodeBegin(FacesContext context) throws IOException
  {
    _fixupFirst();
    super.__encodeBegin(context);
  }

  // make sure the current range exists on the model:
  // see bug 4143852:
  private void _fixupFirst()
  {
    int first = getFirst();
    // if we are starting from row zero then there is no problem:
    if (first == 0)
      return;

    // Negative "first" makes no sense. Given the logic below,
    // it forces iterator to scroll to the end unnecessarily.
    if (first < 0)
    {
      setFirst(0);
      return;
    }

    CollectionModel model = getCollectionModel();
    int oldIndex = model.getRowIndex();
    try
    {
      model.setRowIndex(first);
      // if the starting row doesn't exist then we need to scroll back:
      if (!model.isRowAvailable())
      {
        int size = model.getRowCount();
        int rows = getRows();
        // if the rowCount is unknown OR
        //    the blockSize is show all OR
        //    there are fewer rows than the blockSize on the table
        // then start from the beginning:
        if ((size <= 0) || (rows <= 0) || (size <= rows))
          first = 0;
        else
        {
          // scroll to the last page:
          first = size - rows;
          model.setRowIndex(first);
          // make sure the row is indeed available:
          if (!model.isRowAvailable())
          {
            // row is not available. this happens when getRowCount() lies.
            // Some DataModel implementations seem to have rowCount methods which
            // lie. see bug 4157186
            first = 0;
          }
        }
        setFirst(first);
      }
    }
    finally
    {
      model.setRowIndex(oldIndex);
    }
  }
}
