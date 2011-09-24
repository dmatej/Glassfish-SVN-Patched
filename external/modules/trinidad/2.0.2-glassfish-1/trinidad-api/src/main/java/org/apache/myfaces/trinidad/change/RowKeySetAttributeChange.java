package org.apache.myfaces.trinidad.change;

import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.component.ContextCallback;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;

/**
 * Handles RowKeySetAttribute changes, which need to be handled specially because they are mutable
 * and programmers assume that the instances don't change.
 * 
 * Cases that we need to worry about:
 * 1) old value is null
 * 2) new value is null (should clear existing RowKeySet if possible)
 * 3) old value = new value
 * 4) new value is a ValueExpression or ValueBinding
 * 5) old value is a ValueExpression that needs to be evaluated in context
 * 6) RowKeySet is internally bound to a model that needs to be evaluated in context
 */
public final class RowKeySetAttributeChange extends AttributeComponentChange
{
  public RowKeySetAttributeChange(String clientId,  String propertyName, Object value)
  {
    super(propertyName, value);
    
    if ((clientId == null) || (clientId.length() == 0))
      throw new IllegalArgumentException("No clientId specified");

    _clientId = clientId;
  }

  @Override
  @SuppressWarnings("deprecation")
  public void changeComponent(UIComponent component)
  {
    Map<String, Object> attributeMap = component.getAttributes();

    Object newAttributeValue = getAttributeValue();
    String attrName  = getAttributeName();
    
    if ((newAttributeValue instanceof RowKeySet) || (newAttributeValue == null))
    {
      // Specially handle RowKeySet case by replacing the contents of the RowKeySet in-place
      // rather than replacing the entire object.  This keeps the mutable object instance from
      // changing
      _updateRowKeySetInPlace(component, attrName, (RowKeySet)newAttributeValue);
    }
    else if (newAttributeValue instanceof ValueExpression)
    {
      // if the new attribute value is a ValueExpession, set it and remove the old value
      // so that the ValueExpression takes precedence
      component.setValueExpression(attrName, (ValueExpression)newAttributeValue);
      attributeMap.remove(attrName);
    }
    else if (newAttributeValue instanceof ValueBinding)
    {
      // if the new attribute value is a ValueBinding, set it and remove the old value
      // so that the ValueBinding takes precedence
      component.setValueBinding(attrName, (ValueBinding)newAttributeValue);
      attributeMap.remove(attrName);
    }
    else
    {
      // perform the default behavior
      attributeMap.put(attrName, newAttributeValue);
    }
  }

  private void _updateRowKeySetInPlace(UIComponent component, String attrName, RowKeySet newValue)
  {
    ValueExpression oldExpression = component.getValueExpression(attrName);
    
    // due to bug in how the trinidad table and tree handle their RowKeySets, always use
    // invoke on component and get the old value in context all of the time for now rather
    // than trying to get the value directly if we don't have an expression
    //use EL to get the oldValue and then determine whether we need to update in place
    final FacesContext context = FacesContext.getCurrentInstance();
                
    context.getViewRoot().invokeOnComponent(
      context,
      _clientId,
      new GetOldValueAndUpdate(oldExpression, attrName, newValue));
  }
    
  /**
   * Get the oldValue in context and update it in context
   */
  private static final class GetOldValueAndUpdate implements ContextCallback
  {
    public GetOldValueAndUpdate(ValueExpression expression, String attributeName, RowKeySet newKeySet)
    {
      _expression = expression;
      _attributeName = attributeName;
      _newKeySet  = newKeySet;
    }
    
    public void invokeContextCallback(FacesContext context,
                                      UIComponent target)
    {
      Object oldValue;
      
      // due to bug in how tables and trees handle RowKeySet, temporarily support getting the
      // old value in context, even when we don't have a value expression
      if (_expression != null)
        oldValue = _expression.getValue(context.getELContext());
      else
        oldValue = target.getAttributes().get(_attributeName);
      
      // update the old KeySet with the old and new values
      _updateKeySet(target, oldValue);
    }

    private void _updateKeySet(UIComponent component, Object oldValue)
    {
      // check for equality because otherwise we would clear ourselves and end up empty
      if (oldValue != _newKeySet)
      {
        // if the old value is a RowKeySet, we can replace in place
        if (oldValue instanceof RowKeySet)
        {
          RowKeySet oldKeySet = (RowKeySet)oldValue;
          
          try
          {
            oldKeySet.clear();
            
            if (_newKeySet != null)
            {
              oldKeySet.addAll(_newKeySet);
            }
          }
          catch (Exception e)
          {
            _LOG.warning("FAILED_ROWKEYSETATTRIBUTECHANGE", e);
            return;
          }
        }
        else
        {
          // if the oldKeySet is null, just set the new keySet
          component.getAttributes().put(_attributeName, _newKeySet);
        }
      }    
    }
    
    private final ValueExpression _expression;
    private final String _attributeName;
    private final RowKeySet _newKeySet;
  }

  private static final long serialVersionUID = 1L;
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RowKeySetAttributeChange.class);
  private final String _clientId;
}
