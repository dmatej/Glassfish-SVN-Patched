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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.MutableUINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.collection.UINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeMap;
import org.apache.myfaces.trinidadinternal.ui.composite.RootChildBoundValue;
import org.apache.myfaces.trinidadinternal.ui.composite.RootUINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.AndBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.IfBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.NotBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.OrBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.trinidadinternal.ui.laf.base.TreeWalker;
import org.apache.myfaces.trinidadinternal.ui.laf.base.TreeWalkerUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.UseAccessibilityBoundValue;
import org.apache.myfaces.trinidadinternal.ui.path.Path;


/**
 *
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PageLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:29 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class PageLayoutRenderer extends UINodeRenderer
                  implements UIConstants, BaseDesktopConstants
{

  private static final String _SKIP_PAST_NAVIGATION = "af_panelPage.SKIP_PAST_NAVIGATION";
  private static final String _ORA_HIDE_SKIP_NAVI_TEXT = "p_OraHideSkipNaviText";

  @Override
  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _INSTANCE;
  }

  private static void _setPoppedChild(
    MutableUINode node,
    String        childName
    )
  {
    node.setNamedChild(childName,
                       ContextPoppingUINode.getUINode(childName));
  }


  private static UINode _createCompositeUINode()
  {

    //
    // Create the page header area
    //
    MarlinBean globalHeaders = _sCreateGlobalHeader();

    // If no start child add quickSearch
    BoundValue hasNavigation3 = BaseDesktopUtils.createIsRenderedBoundValue(NAVIGATION3_CHILD);
    BoundValue hasSearch =
                      BaseDesktopUtils.createIsRenderedBoundValue(SEARCH_CHILD);
    BoundValue hasSearchAndNoNavigation3 =
                              new AndBoundValue( new NotBoundValue(hasNavigation3),
                                                 hasSearch);
    BoundValue hasNavigation3AndSearch = new AndBoundValue( hasNavigation3, hasSearch);

    MarlinBean qsNoStartFlow = new MarlinBean(FLOW_LAYOUT_NAME);
    UINode quickSearch = ContextPoppingUINode.getUINode(SEARCH_CHILD);
    qsNoStartFlow.addIndexedChild( quickSearch );
    qsNoStartFlow.setAttributeValue(RENDERED_ATTR,
                                    hasSearchAndNoNavigation3);

    MarlinBean pageHeader = _sCreatePageHeader(globalHeaders,
                                                         qsNoStartFlow);


    // @todo if there is a navigation3 child, the search should go in the sideBar,
    // however we need a search component that will lay itself out properly
    // for the reduced space of a sideBar before we do that.
    MarlinBean startRegionLayout = new MarlinBean(SIDE_BAR_NAME);
    startRegionLayout.addIndexedChild(
                                ContextPoppingUINode.getUINode(NAVIGATION3_CHILD));

    startRegionLayout.setAttributeValue(RENDERED_ATTR, hasNavigation3);

    //
    // Create the page content area
    //

    // If a start child quickSearch rendered on the top of content area
    MarlinBean qsStartStack = new MarlinBean(STACK_LAYOUT_NAME);
    MarlinBean qsSeparator = new MarlinBean(SEPARATOR_NAME);
    qsStartStack.addIndexedChild(quickSearch);
    qsStartStack.addIndexedChild(qsSeparator);
    qsStartStack.setAttributeValue(RENDERED_ATTR, hasNavigation3AndSearch);


    //
    // Create layout used for locators at the top of the page
    //
    MarlinBean locatorLayout = new MarlinBean(STACK_LAYOUT_NAME);

    MarlinBean topSpacer = new MarlinBean(SPACER_NAME);
    topSpacer.setAttributeValue(HEIGHT_ATTR,"5");

    BoundValue locatorBoundValue =
      RootChildBoundValue.getBoundValue(LOCATION_CHILD);

    BoundValue isLocatorRendered =
      BaseDesktopUtils.createIsRenderedBoundValue(locatorBoundValue);

    BoundValue isUserInfoRendered =
      BaseDesktopUtils.createIsRenderedBoundValue(INFO_USER_CHILD);

    BoundValue isEndRendered =
      BaseDesktopUtils.createIsRenderedBoundValue(INFO_SUPPLEMENTAL_CHILD);

    BoundValue hasTrainBean =
     new PageLayoutRenderer.TreeWalkerBoundValue( locatorBoundValue,
                                                  PROCESS_TRAIN_NAME );

    BoundValue isTrainRendered =
      new AndBoundValue(isLocatorRendered,
                        hasTrainBean);


    // only render the top spacer if the Locator is rendered
    topSpacer.setAttributeValue(RENDERED_ATTR, isLocatorRendered);

    MutableUINode locatorUserInfoLayout = _sCreateLocatorUserInfoLayout(
                                        isEndRendered,
                                        isLocatorRendered,
                                        isUserInfoRendered);

    // the train separator is only rendered if the train is rendered
    MarlinBean trainSeparator = new MarlinBean(SEPARATOR_NAME);
    trainSeparator.setAttributeValue(RENDERED_ATTR, isTrainRendered);

    locatorLayout.addIndexedChild(topSpacer);
    locatorLayout.addIndexedChild(qsStartStack);
    locatorLayout.addIndexedChild(locatorUserInfoLayout);
    locatorLayout.addIndexedChild(trainSeparator);
    locatorLayout.addIndexedChild(
                             ContextPoppingUINode.getUINode( MESSAGES_CHILD ));

    //
    // Create the content footer
    //

    // if content footer child has returnNavigation child and/or
    // pageButtons child use these, otherwise
    // if the content footer child is a MarlinBean or
    // has a contentMarlinBean as a descendent
    // add it directly, otherwise create a MarlinBean and add the
    // content footer child as its indexed child

    MarlinBean contentFooterSwitcher = new MarlinBean(SWITCHER_NAME);

    BoundValue contentFooterBoundValue =
                       RootChildBoundValue.getBoundValue(CONTENT_FOOTER_CHILD);

    // Is there a MarlinBean inside of the CONTENT_FOOTER_CHILD
    BoundValue hasContentMarlinBean =
                       new PageLayoutRenderer.TreeWalkerBoundValue( contentFooterBoundValue,
                                                 CONTENT_FOOTER_NAME );

    // Create MarlinBean using what's in ACTIONS_CHILD
    // and INFO_RETURN_CHILD
    MarlinBean pageNavContentFooter = new MarlinBean(CONTENT_FOOTER_NAME);


    pageNavContentFooter.addIndexedChild(
                       ContextPoppingUINode.getUINode(ACTIONS_CHILD));
    pageNavContentFooter.setNamedChild(START_CHILD,
                       ContextPoppingUINode.getUINode(INFO_RETURN_CHILD));

    // Create MarlinBean using what's in CONTENT_FOOTER_CHILD
    MarlinBean contentMarlinBean = new MarlinBean(CONTENT_FOOTER_NAME);
    contentMarlinBean.addIndexedChild(
                       ContextPoppingUINode.getUINode(CONTENT_FOOTER_CHILD));

    // Use content footer created with
    // ACTIONS_CHILD and INFO_RETURN_CHILD
    contentFooterSwitcher.setNamedChild( _HAS_PAGE_NAV, pageNavContentFooter);

    // Use content footer already in CONTENT_FOOTER_CHILD
    contentFooterSwitcher.setNamedChild(
                         _HAS_CONTENT_FOOTER,
                         ContextPoppingUINode.getUINode(CONTENT_FOOTER_CHILD));

    // Use content footer created with what's in CONTENT_FOOTER_CHILD
    contentFooterSwitcher.setNamedChild( _NO_CONTENT_FOOTER, contentMarlinBean);

    // figure out which one to use:
    //     First Choice: Use the one created with ACTIONS_CHILD
    //                   and INFO_RETURN_CHILD if these children exist.
    //    Second Choice: Use one already in CONTENT_FOOTER_CHILD.
    //     Third Choice: Use one created with what's in CONTENT_FOOTER_CHILD
    BoundValue pageButtonsExistBV =
                      BaseDesktopUtils.createIsRenderedBoundValue(ACTIONS_CHILD);
    BoundValue returnNavExistsBV =
                  BaseDesktopUtils.createIsRenderedBoundValue(INFO_RETURN_CHILD);

    OrBoundValue newChildrenExist = new OrBoundValue( pageButtonsExistBV,
                                                      returnNavExistsBV);

    IfBoundValue hasContentFooterBV = new IfBoundValue( hasContentMarlinBean,
                                                        _HAS_CONTENT_FOOTER,
                                                        _NO_CONTENT_FOOTER );

    IfBoundValue childNameBV = new IfBoundValue( newChildrenExist,
                                                 _HAS_PAGE_NAV,
                                                 hasContentFooterBV);
    contentFooterSwitcher.setAttributeValue(CHILD_NAME_ATTR, childNameBV);

    // create footnote region above contentFooterSwitcher in StackLayout
    MarlinBean contentFooterStackLayout = new MarlinBean(STACK_LAYOUT_NAME);
    contentFooterStackLayout.addIndexedChild(
                  ContextPoppingUINode.getUINode(INFO_FOOTNOTE_CHILD));
    contentFooterStackLayout.addIndexedChild(contentFooterSwitcher);

    //
    // Create the footer area
    //


    MarlinBean footer = _sCreateFooterBean();

    //
    // Build up the root node
    //
    MarlinBean endChildSeparator = new MarlinBean(SPACER_NAME);
    endChildSeparator.setAttributeValue(WIDTH_ATTR, "5");
    endChildSeparator.setAttributeValue(HEIGHT_ATTR, "1");

    // the end child separator is only rendered if the end child is rendered
    endChildSeparator.setAttributeValue(RENDERED_ATTR,
                                        isEndRendered);

    // the userInfo is rendered in the end area if the end child is rendered
    // otherwise it is rendered in the locator area
    MutableUINode endRegionLayout = _sCreateEndLayout(isEndRendered,
                                                      isUserInfoRendered);

    UINodeList rootNodeList = RootUINodeList.getNodeList();

    //
    // Create content of PageLayout to use when we have no page title
    //
    MutableUINode noTitleContentLayout = new MarlinBean(STACK_LAYOUT_NAME);
    noTitleContentLayout.addIndexedChild(_sCreateContentTop(true));
    noTitleContentLayout.addIndexedChild(_createContentLayout(rootNodeList));


    // this is the name of the anchor used to skip the navigational area of a
    // pageLayout and jump to the content area.
    final String skipNavigationAnchor = "TheContent";

    MarlinBean content =
      new MarlinBean(BORDER_LAYOUT_NAME);

    content.setNamedChild(INNER_TOP_CHILD, locatorLayout);
    content.setNamedChild(INNER_BOTTOM_CHILD, contentFooterStackLayout);
    content.setNamedChild(INNER_END_CHILD, endChildSeparator);
    content.setNamedChild(START_CHILD, startRegionLayout);
    content.setNamedChild(END_CHILD, endRegionLayout);


    // Add an anchor at the start of the content, so that screen reader
    // users may jump to this location from a link at the top of the page.
    MarlinBean anchor = new MarlinBean(LINK_NAME);
    anchor.setAttributeValue(NAME_ATTR, skipNavigationAnchor);
    anchor.setAttributeValue(RENDERED_ATTR,
                             UseAccessibilityBoundValue.sharedInstance());
    content.addIndexedChild(anchor);

    content.addIndexedChild(noTitleContentLayout);

    MarlinBean compositeRoot = new MarlinBean(FLOW_LAYOUT_NAME);
    // delegate all of the attributes to the RootAttributeMap
    compositeRoot.setAttributeMap(RootAttributeMap.getAttributeMap());
    compositeRoot.addIndexedChild(
        _sCreateSkipNavigationLink(skipNavigationAnchor));
    compositeRoot.addIndexedChild(pageHeader);
    compositeRoot.addIndexedChild(content);
    compositeRoot.addIndexedChild(footer);
    return compositeRoot;
  }

  /**
   * create a link just before the page layout which a screen-reader user may
   * use to skip all the navigation links and jump right to the content.
   */
  private static MarlinBean _sCreateSkipNavigationLink(
    String skipNavigationAnchor
    )
  {
    MarlinBean skipNavigationLink = new MarlinBean(LINK_NAME);
    BoundValue skipNavigationText =
      new SkinTranslatedBoundValue(_SKIP_PAST_NAVIGATION);
    skipNavigationLink.setAttributeValue(DESTINATION_ATTR, "#"+skipNavigationAnchor);
    skipNavigationLink.setAttributeValue(UIConstants.TEXT_ATTR,
                                         skipNavigationText);
    skipNavigationLink.setAttributeValue(UIConstants.RENDERED_ATTR,
      new AndBoundValue(
        new PageLayoutRenderer.NetscapeBoundValue(Boolean.FALSE,
                               Boolean.TRUE),
        UseAccessibilityBoundValue.sharedInstance()));
    skipNavigationLink.setAttributeValue(STYLE_CLASS_ATTR,_ORA_HIDE_SKIP_NAVI_TEXT);
    return skipNavigationLink;
  }

  /**
   * Create layout used for global headers; render the user one by default,
   * but render another one if there's no user one but there is a sideNav, and
   * a third if there's no page header or side nav.
   */
  private static MarlinBean _sCreateGlobalHeader()
  {

    // create an empty global header
    MarlinBean emptyGlobalHeader = new MarlinBean(GLOBAL_HEADER_NAME);
    // bind rendered to false if there is page header child
    BoundValue rendered = new NotBoundValue(
           BaseDesktopUtils.createIsRenderedBoundValue(NAVIGATION2_CHILD));
    emptyGlobalHeader.setAttributeValue(RENDERED_ATTR,rendered);

    MarlinBean globalHeaders = new MarlinBean(STACK_LAYOUT_NAME);
    globalHeaders.addIndexedChild(
              ContextPoppingUINode.getUINode(NAVIGATION2_CHILD));
    globalHeaders.addIndexedChild(emptyGlobalHeader);
    return globalHeaders;
  }


  /**
   * create the header area for the top of the page
   */
  private static MarlinBean _sCreatePageHeader(
    UINode globalHeader,
    UINode quickSearch
    )
  {
    MarlinBean pageHeader = new MarlinBean(PAGE_HEADER_LAYOUT_NAME);

    _setPoppedChild(pageHeader, NAVIGATION1_CHILD);
    _setPoppedChild(pageHeader, ADVERTISEMENT_LARGE_CHILD);
    _setPoppedChild(pageHeader, ADVERTISEMENT_MEDIUM_CHILD);
    _setPoppedChild(pageHeader, BRANDING_CHILD);
    _setPoppedChild(pageHeader, BRANDING_COOPERATIVE_CHILD);
    _setPoppedChild(pageHeader, BRANDING_APP_CHILD);
    _setPoppedChild(pageHeader, BRANDING_APP_CONTEXTUAL_CHILD);
    _setPoppedChild(pageHeader, NAVIGATION_GLOBAL_CHILD);
    _setPoppedChild(pageHeader, MENU_SWITCH_CHILD);
    pageHeader.setNamedChild(SEARCH_CHILD, quickSearch);
    pageHeader.setNamedChild(NAVIGATION2_CHILD, globalHeader);

    BoundValue chromeTypeBV =
      RootAttributeBoundValue.getBoundValue(CHROME_TYPE_ATTR);
    pageHeader.setAttributeValue(CHROME_TYPE_ATTR, chromeTypeBV);
    return pageHeader;
  }

  /**
   * Creates the footer
   */
  private static MarlinBean _sCreateFooterBean()
  {
    MarlinBean footer = new MarlinBean(FOOTER_NAME);
    _setPoppedChild(footer, APP_COPYRIGHT_CHILD);
    _setPoppedChild(footer, APP_PRIVACY_CHILD);
    _setPoppedChild(footer, APP_ABOUT_CHILD);
    return footer;
  }

  private static MutableUINode _createContentLayout(
    UINodeList nodeList
    )
  {
    MarlinBean contentLayout = new MarlinBean(STACK_LAYOUT_NAME);
    contentLayout.setIndexedNodeList(nodeList);

    return contentLayout;
  }

  private static UINode _sCreateContentTop(
    boolean useContextSwitcher
  )
  {
    MarlinBean table = new MarlinBean(TABLE_LAYOUT_NAME);
    table.setAttributeValue(WIDTH_ATTR, "100%");
    table.setAttributeValue(CELL_PADDING_ATTR, "0");
    table.setAttributeValue(CELL_SPACING_ATTR, "0");

    MarlinBean row = new MarlinBean(ROW_LAYOUT_NAME);
    row.setAttributeValue(V_ALIGN_ATTR, "top");
    table.addIndexedChild(row);

    // cell to add page status to
    MarlinBean statusCell = new MarlinBean(CELL_FORMAT_NAME);
    statusCell.setAttributeValue(WIDTH_ATTR, "100%");
    MarlinBean stack = new MarlinBean(STACK_LAYOUT_NAME);

    if ( useContextSwitcher)
    {
      stack.addIndexedChild(
                     ContextPoppingUINode.getUINode( CONTEXT_SWITCHER_CHILD));
    }

    stack.addIndexedChild(
                      ContextPoppingUINode.getUINode(INFO_STATUS_CHILD));
    statusCell.addIndexedChild(stack);
    row.addIndexedChild(statusCell);

    return table;
  }

  /**
  * Creates the cell with the user info in it
  */
  private static MarlinBean _sCreateUserInfoCell()
  {
    // cell to add user info to
    MarlinBean userInfoCell = new MarlinBean(CELL_FORMAT_NAME);
    userInfoCell.setAttributeValue(H_ALIGN_ATTR, "end");
    userInfoCell.addIndexedChild(
                      ContextPoppingUINode.getUINode(INFO_USER_CHILD));
    return userInfoCell;
  }

  /**
  * Creates the table with one row
  * which has the locator and the user info in it
  */
  private static MarlinBean _sCreateLocatorUserInfoTable()
  {
    MarlinBean table = new MarlinBean(TABLE_LAYOUT_NAME);
    table.setAttributeValue(WIDTH_ATTR, "100%");

    MarlinBean row = new MarlinBean(ROW_LAYOUT_NAME);
    table.addIndexedChild(row);
    // add cells to the row:

    // add a  cell with the locator child
    MarlinBean locatorCell = new MarlinBean(CELL_FORMAT_NAME);
    locatorCell.addIndexedChild(
                      ContextPoppingUINode.getUINode(LOCATION_CHILD));
    row.addIndexedChild(locatorCell);
    // figure out if the locator region should be rendered

    // add a cell with some horizontal space
    MarlinBean spacerCell = new MarlinBean(CELL_FORMAT_NAME);
    MarlinBean spacer = new MarlinBean(SPACER_NAME);
    spacer.setAttributeValue(WIDTH_ATTR, "5");
    spacer.setAttributeValue(HEIGHT_ATTR, "1");
    spacerCell.addIndexedChild(spacer);
    row.addIndexedChild(spacerCell);

    // add a cell with the user info child
    MarlinBean userInfoCell = _sCreateUserInfoCell();
    row.addIndexedChild(userInfoCell);

    return table;
  }

  /**
  * Creates the table with a user info row, a spacer row, and an end row
  */
  private static MarlinBean _sCreateUserInfoEndTable()
  {
    MarlinBean table = new MarlinBean(TABLE_LAYOUT_NAME);
    table.setAttributeValue(WIDTH_ATTR, "100%");
    // add rows:

    // user info row
    MarlinBean userInfoRow = new MarlinBean(ROW_LAYOUT_NAME);
    table.addIndexedChild(userInfoRow);
    MarlinBean userInfoCell = _sCreateUserInfoCell();
    userInfoRow.addIndexedChild(userInfoCell);
    // spacer row
    MarlinBean spacerRow = new MarlinBean(ROW_LAYOUT_NAME);
    table.addIndexedChild(spacerRow);
    MarlinBean spacer = new MarlinBean(SPACER_NAME);
    spacer.setAttributeValue(HEIGHT_ATTR,"5");
    spacerRow.addIndexedChild(spacer);
    // end child row
    MarlinBean endRow = new MarlinBean(ROW_LAYOUT_NAME);
    table.addIndexedChild(endRow);
    MarlinBean endCell = new MarlinBean(CELL_FORMAT_NAME);
    endCell.addIndexedChild(
                      ContextPoppingUINode.getUINode(INFO_SUPPLEMENTAL_CHILD));
    endRow.addIndexedChild(endCell);

    return table;
  }

  /**
  * Create layout used for the end child
  * Just render the end child if no user info is rendered
  * otherwise, render a table, with the user info as the first row
  **/
  private static MutableUINode _sCreateEndLayout(
    BoundValue isEndRendered,
    BoundValue isUserInfoRendered
    )
  {
    // render table only if both user info and end region
    MarlinBean userEndTable = _sCreateUserInfoEndTable();
    userEndTable.setAttributeValue(
                      RENDERED_ATTR,
                      new AndBoundValue(isEndRendered, isUserInfoRendered));

    // render end region only if no user info
    MarlinBean endOnlyLayout = new MarlinBean(STACK_LAYOUT_NAME);
    MarlinBean spacer = new MarlinBean(SPACER_NAME);
    spacer.setAttributeValue(HEIGHT_ATTR,"5");
    endOnlyLayout.addIndexedChild(spacer);
    endOnlyLayout.addIndexedChild(ContextPoppingUINode.getUINode(INFO_SUPPLEMENTAL_CHILD));
    endOnlyLayout.setAttributeValue(RENDERED_ATTR,
                            new AndBoundValue(isEndRendered,
                              new NotBoundValue(isUserInfoRendered)));

    // place these two possibilities in a flowLayoutBean
    // one or the other child will be rendered if an end is rendered
    MarlinBean endLayout = new MarlinBean(FLOW_LAYOUT_NAME);
    endLayout.addIndexedChild(userEndTable);
    endLayout.addIndexedChild(endOnlyLayout);

    endLayout.setAttributeValue(RENDERED_ATTR, isEndRendered);
    return endLayout;
  }

  /**
  * Create layout used for the locator/userInfo child
  * Just render the locator if end exists or no userInfo
  * otherwise, render a table, with the userInfo as the first row
  **/
  private static MutableUINode _sCreateLocatorUserInfoLayout(
    BoundValue isEndRendered,
    BoundValue isLocatorRendered,
    BoundValue isUserInfoRendered
    )
  {
    // render table only if userInfo and no end region
    MarlinBean locatorUserTable = _sCreateLocatorUserInfoTable();
    BoundValue isUserInfoAndNoEnd = new AndBoundValue(isUserInfoRendered,
                                    new NotBoundValue(isEndRendered));
    locatorUserTable.setAttributeValue(RENDERED_ATTR, isUserInfoAndNoEnd);

    // render only the locator if locator and (no userInfo or end)
    MarlinBean locatorOnlyLayout = new MarlinBean(FLOW_LAYOUT_NAME);
    locatorOnlyLayout.addIndexedChild(
                        ContextPoppingUINode.getUINode(LOCATION_CHILD));
    BoundValue isEndOrNoUserInfo =
                    new OrBoundValue(isEndRendered,
                      new NotBoundValue(isUserInfoRendered));
    locatorOnlyLayout.setAttributeValue(
                        RENDERED_ATTR,
                          new AndBoundValue(isLocatorRendered,
                                              isEndOrNoUserInfo));
    // place these two possibilities in a flowLayoutBean
    // one or the other child will be rendered
    MarlinBean locatorUserLayout = new MarlinBean(FLOW_LAYOUT_NAME);
    locatorUserLayout.addIndexedChild(locatorUserTable);
    locatorUserLayout.addIndexedChild(locatorOnlyLayout);

    locatorUserLayout.setAttributeValue(
                          RENDERED_ATTR,
                          new OrBoundValue(isLocatorRendered,
                                           isUserInfoAndNoEnd));
    return locatorUserLayout;
  }

  /**
   * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
   */
  @Deprecated
  private static final class NetscapeBoundValue implements BoundValue
  {
    public NetscapeBoundValue(Object ifNetscapeValue, Object ifNotValue)
    {
      _netscapeObj = ifNetscapeValue;
      _elseObj = ifNotValue;
    }

    public Object getValue(UIXRenderingContext context)
    {
      if (context.getAgent().getAgentApplication() ==
          TrinidadAgent.Application.NETSCAPE)
        return _netscapeObj;
      else return _elseObj;
    }

    private final Object _netscapeObj, _elseObj;
  }

  private static final UINode _INSTANCE = _createCompositeUINode();
  private static final String _HAS_CONTENT_FOOTER = "cf";
  private static final String _NO_CONTENT_FOOTER  = "nocf";
  private static final String _HAS_PAGE_NAV       = "pn";

  /**
  * This bound value walks the tree to see if any node has a marlin namespace
  * and the local name passed in and returnsBoolean.TRUE if it finds such
  * a node
  *
  * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
  */
  @Deprecated
  private static class TreeWalkerBoundValue implements BoundValue, TreeWalker
  {

    TreeWalkerBoundValue(
      BoundValue rootBoundValue,
      String     beanLocalName
      )
    {
      if (rootBoundValue == null || beanLocalName == null)
        throw new IllegalArgumentException();

      _rootBoundValue = rootBoundValue;
      _beanLocalName= beanLocalName;

    }

    public Object walkNode(
      UIXRenderingContext context,
      UINode           node,
      Object           previousValue,
      Path             path)
    {
      if ( MARLIN_NAMESPACE.equals(node.getNamespaceURI()) &&
           _beanLocalName.equals(node.getLocalName()))
      {
        Boolean rendered = (Boolean)node.getAttributeValue(context,
                                                           RENDERED_ATTR);
        return Boolean.FALSE.equals(rendered) ? Boolean.FALSE : Boolean.TRUE;
      }

      return previousValue;
    }


    public boolean walkChildren(
      UIXRenderingContext context,
      UINode           node,
      Object           value,
      Path             path)
    {
      if ( value == Boolean.TRUE )
        return true;

      return false;
    }

    public Object getValue(
      UIXRenderingContext context
      )
    {
      UINode child = (UINode)_rootBoundValue.getValue( context );

      if ( child != null )
      {

        if ( MARLIN_NAMESPACE.equals(child.getNamespaceURI()) &&
             _beanLocalName.equals(child.getLocalName()))
        {
          Boolean rendered = (Boolean)child.getAttributeValue(context,
                                                              RENDERED_ATTR);
          return Boolean.FALSE.equals(rendered) ? Boolean.FALSE : Boolean.TRUE;
        }

        try{
          return TreeWalkerUtils.walkTree( context, child, this );
        }
        catch(Exception e){ assert(false); }
      }

      return Boolean.FALSE;

  }
    private BoundValue _rootBoundValue;
    private String     _beanLocalName;

  }

}
