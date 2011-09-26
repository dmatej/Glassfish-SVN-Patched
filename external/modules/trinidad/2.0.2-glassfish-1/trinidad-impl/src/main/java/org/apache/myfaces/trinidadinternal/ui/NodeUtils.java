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
package org.apache.myfaces.trinidadinternal.ui;

import java.util.Iterator;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;


/**
 * Utility methods for working with UINodes.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/NodeUtils.java#0 $) $Date: 10-nov-2005.18:50:15 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NodeUtils implements UIConstants
{
  private NodeUtils()
  {
  }



  /**
   * Return the UIComponent for a node.
   */
  static public UIComponent getUIComponent(UIXRenderingContext context,
                                           UINode node)
  {
    UIComponent component = node.getUIComponent();
    if (component != null)
      return component;

    // Go up to the parent context and see if the node there
    // is attached to any particular UIComponent;  but only
    // do that for the "root" of any particular tree.
    if (context.getAncestorNodeCount() >= 1)
    {
      UIXRenderingContext parentContext = context.getParentContext();
      if (parentContext != null)
      {
        UINode parentNode = parentContext.getAncestorNode(0);
        if (parentNode != null)
        {
          component = getUIComponent(parentContext, parentNode);
          if (component != null)
            return component;
        }
      }
    }

    return null;
  }


  /**
   * Utility method to set the text as well as any embedded access key on
   * the node.  The access key is indicator by placing an ampersand character
   * before the character to use as the access key.  Ampersands that you
   * wish to have appear in the text are escaped by using a double ampersand.
   * <p>
   * The stripped text will be set on the UINode using the
   * <code>UIConstants.TEXT_ATTR</code> key.
   * <p>
   * @param node UINode to set the text and access key on
   * @param embeddedText String containing accessKey escaped with an ampersand
   */
  public static void setTextAndAccessKey(
    MutableUINode  node,
    String         embeddedText
    )
  {
    setTextAndAccessKey(node, embeddedText, TEXT_ATTR);
  }

  /**
   * Utility method to set the text as well as any embedded access key on
   * the node.  The access key is indicator by placing an ampersand character
   * before the character to use as the access key.  Ampersands that you
   * wish to have appear in the text are escaped by using a double ampersand.
   * <p>
   * This method is useful for UINodes that use an attribute other than
   * <code>UIConstants.TEXT_ATTR</code> to indicate the text of the node.
   * An example of such a UINode type are the InlineMessages.
   * <p>
   * @param node UINode to set the text and access key on
   * @param embeddedText String containing accessKey escaped with an ampersand
   * @param textAttributeKey Name of attribute to set the stripped text with.
   */
  public static void setTextAndAccessKey(
    MutableUINode    node,
    String           embeddedText,
    AttributeKey     textAttributeKey
    )
  {
    // set the acess key, if any
    int accessKeyIndex = StringUtils.getMnemonicIndex(embeddedText);

    if (accessKeyIndex != StringUtils.MNEMONIC_INDEX_NONE)
    {
      // set the acesskey on the node
      node.setAttributeValue(
                        ACCESS_KEY_ATTR,
                        Character.valueOf(embeddedText.charAt(accessKeyIndex + 1)));
    }

    // set the stripped text on the node using the appropriate attribute name
    node.setAttributeValue(textAttributeKey,
                           StringUtils.stripMnemonic(embeddedText));
  }


  /**
   * Returns the root UINode in the specified RenderingContext
   */
  public static UINode getRootUINode(
    UIXRenderingContext context
    )
  {
    return context.getAncestorNode(context.getAncestorNodeCount() - 1);
  }


  /**
   * Returns the root UINode in a stack of RenderingContexts
   */
  public static UINode getStackRootUINode(
    UIXRenderingContext context
    )
  {
    return getRootUINode(_getRootRenderingContext(context));
  }


  /**
   * Returns true if the specified UINode is a rendered ancestor of the current
   * rendering UINode
   */
  public static boolean isRenderedAncestor(
    UIXRenderingContext context,
    UINode           ancestorNode
    )
  {
    int ancestorCount = context.getRenderedAncestorNodeCount();

    for (int i = 1; i < ancestorCount; i++)
    {
      UINode curr = context.getRenderedAncestorNode(i);

      if (curr == ancestorNode)
      {
        return true;
      }
    }

    return false;
  }


  /**
   * Appends the array of UINodes to the end of the list of indexed
   * children on the parentNode.
   * <p>
   * @see MutableUINode#addIndexedChild
   */
  public static void addIndexedChildren(
    MutableUINode parentNode,
    UINode[]      indexedChildren
    )
  {
    if (parentNode != null)
    {
      int childCount = (indexedChildren != null)
                         ? indexedChildren.length
                         : 0;

      for (int i = 0; i < childCount; i++)
      {
        parentNode.addIndexedChild(indexedChildren[i]);
      }
    }
  }


  /**
   * Appends the Iterator of UINodes to the end of the list of indexed
   * children on the parentNode.
   * <p>
   * @see MutableUINode#addIndexedChild
   */
  public static void addIndexedChildren(
    MutableUINode    parentNode,
    Iterator<UINode> indexedChildren
    )
  {
    if ((parentNode != null) && (indexedChildren != null))
    {
      while (indexedChildren.hasNext())
      {
        UINode currNode = indexedChildren.next();
        parentNode.addIndexedChild(currNode);
      }
    }
  }


  /**
   * Returns the first attribute value for the specified <b>attrName</b>, found
   * be performing a preorder (depth-first) search of the <b>startNode</b>
   * and its children.
   */
  public static Object getPreorderDescendentAttributeValue(
    UIXRenderingContext context,
    UINode           startNode,
    AttributeKey     attrKey
    )
  {
    Object value = startNode.getAttributeValue(context, attrKey);

    if (value == null)
    {
      int childCount = startNode.getIndexedChildCount(context);

      for (int i = 0; i < childCount; i++)
      {
        UINode childNode = startNode.getIndexedChild(context, i);

        value = getPreorderDescendentAttributeValue(
                                         context,
                                         childNode,
                                         attrKey);

        if (value != null)
        {
          break;
        }
      }
    }

    return value;
  }


  /**
   * Returns the root RenderingContext in a stack of RenderingContexts
   */
  private static UIXRenderingContext _getRootRenderingContext(
    UIXRenderingContext context
    )
  {
    UIXRenderingContext parentContext = context;

    while (parentContext != null)
    {
      context = parentContext;
      parentContext = context.getParentContext();
    }

    return context;
  }
}
