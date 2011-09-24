package org.apache.myfaces.trinidad.model;

/**
 * Creates a CollectionModel whose row keys are defined by a unique data property in the model.
 */
public class RowKeyPropertyModel extends SortableModel
{
  /**
   * Creates a RowKeyPropertyModel.
   *
   * @param model The underlying model. If necessary, this will be converted into a {@link DataModel}
   * @param rowKeyProperty The property by which the row key can be accessed. Row key value must be unique
   */
  public RowKeyPropertyModel(Object model, String rowKeyProperty)
  {
    super(model);
    _rowKeyProperty = rowKeyProperty;
  }
  
  /**
   * No arg constructor for use as a managed-bean.
   * Must call {@link #setWrappedData} and {@link #setRowKeyProperty} before using this instance.
   */
  public RowKeyPropertyModel()
  {
    super();
  }

  /**
   * Gets the row key for the current row
   * @return row key or null if model is not on any row
   */
  public Object getRowKey()
  {
    if (isRowAvailable())
    {
      Object rowKey = _getRowKey();
      return rowKey;
    }
    else
    {
      return null;
    }
  }

  /**
   * Moves the model to the row identified by the key.
   * @param key target row key
   */
  public void setRowKey(Object key)
  {
    if (key == null)
    {
      setRowIndex(-1);
      return;
    }

    if (getRowKey() != null && getRowKey().equals(key))
      return;
    
    for (int i = 0; i < getRowCount(); i++)
    {
      setRowIndex(i);
      Object prop = getRowKey();
      if (key.equals(prop))
      {
        return;
      }
    }

    // if we didn't find an element with a matching key,
    // then don't make any rows current
    setRowIndex(-1);
  }

  /**
   * Gets the row key property name for this model
   * @return row key property name
   */
  public String getRowKeyProperty()
  {
    return _rowKeyProperty;
  }

  /**
   * Sets the row key property for this model
   * @param rowKeyProperty row key property to set
   */
  public void setRowKeyProperty(String rowKeyProperty)
  {
    _rowKeyProperty = rowKeyProperty;
  }

  /**
   * gets the row key for the given row by resolving the _rowKeyProperty
   * @param row row to retrieve the row key for
   * @return row key value
   */
  protected Object getRowKey(Object row)
  {
    assert (_rowKeyProperty != null);
    return __resolveProperty(row, _rowKeyProperty);
  }

  /**
   * gets the row key for current row by resolving the _rowKeyProperty
   * @return
   */
  private Object _getRowKey()
  {
    Object data = getRowData();
    
    assert (_rowKeyProperty != null);
    return __resolveProperty(data, _rowKeyProperty);
  }
  
  private String _rowKeyProperty;

}
