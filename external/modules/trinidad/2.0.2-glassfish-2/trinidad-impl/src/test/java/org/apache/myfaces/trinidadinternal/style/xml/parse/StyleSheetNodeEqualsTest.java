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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;

/**
 * Test the getStyleSheetId() method on the StyleSheetNode object.
 * The id of the StyleSheetNode object is used to create a StyleSheetDocument
 * id.
 */
public class StyleSheetNodeEqualsTest extends TestCase
{
  public StyleSheetNodeEqualsTest(String name)
  {
    super(name);
  }

  /**
   * test the IncludePropertyNode hashCode and equals
   */
  public void testIncludePropertyNodeEquals() throws Exception
  {
    IncludePropertyNode node =
      new IncludePropertyNode(null, "AFDefaultColor", "background-color", "color");
    IncludePropertyNode anotherNode =
      new IncludePropertyNode(null, "AFDefaultColor", "background-color", "color");
    IncludePropertyNode unequalNode =
      new IncludePropertyNode(null, "AFDefaultColor", "background-color", "border-color");

    assertEquals(node.hashCode() == anotherNode.hashCode(), true);
    assertEquals(node.equals(anotherNode), true);
    assertEquals(anotherNode.equals(node), true);
    assertEquals(node.equals(unequalNode), false);
    assertEquals(node.equals("Hello"), false);
  }

  /**
   * test the IncludeStyleNode hashCode and equals
   */
  public void testIncludeStyleNodeEquals() throws Exception
  {
    IncludeStyleNode node =
      new IncludeStyleNode("name", null);
    IncludeStyleNode anotherNode =
      new IncludeStyleNode("name", null);
    IncludeStyleNode unequalNode =
      new IncludeStyleNode(null, "anotherSelector");

    assertEquals(node.hashCode() == anotherNode.hashCode(), true);
    assertEquals(node.equals(anotherNode), true);
    assertEquals(anotherNode.equals(node), true);
    assertEquals(node.equals(unequalNode), false);
    assertEquals(node.equals("Hello"), false);

  }

  /**
   * test the StyleNode hashCode and equals
   */
  public void testStyleNodeEquals() throws Exception
  {

    StyleNode afDefaultFontNode = getAfDefaultFontStyleNode();

    StyleNode afDefaultFontNodeReset = getAfDefaultFontResetStyleNode();

    StyleNode afOutputLabelNode = getOutputLabelStyleNode();

    StyleNode anotherOutputLabelNode = getAnotherOutputLabelStyleNode();

    assertEquals(anotherOutputLabelNode.hashCode() == afOutputLabelNode.hashCode(), true);

    assertEquals(anotherOutputLabelNode.equals(afOutputLabelNode), true);
    assertEquals(afOutputLabelNode.equals(anotherOutputLabelNode), true);

    assertEquals(afDefaultFontNode.equals(anotherOutputLabelNode), false);

    assertEquals(afDefaultFontNode.equals(afDefaultFontNodeReset), false);
    assertEquals(afDefaultFontNodeReset.equals(afDefaultFontNode), false);
    assertEquals(afDefaultFontNodeReset.equals("Hello"), false);

  }

  public void testStyleSheetNodeEquals() throws Exception
  {
    StyleNode[] styleSheetOneNodes = {getAfDefaultFontStyleNode(),
                                      getAfDefaultFontResetStyleNode(),
                                      getOutputLabelStyleNode(),
                                      getAnotherOutputLabelStyleNode()};
    StyleNode[] anotherStyleSheetOneNodes =
                                      {getAfDefaultFontStyleNode(),
                                      getAfDefaultFontResetStyleNode(),
                                      getOutputLabelStyleNode(),
                                      getAnotherOutputLabelStyleNode()};

    List<IconNode> iconNodes = _getIconNodes();
    List<IconNode> anotherIconNodes = _getIconNodes();

    // create locales arrays
    Set<Locale> localeSet = getLocalesSet();
    Set<Locale> diffOrderLocalesSet = getDiffOrderLocalesSet();

    // create a browsers map
    String browserSelector = "netscape and (version:5), netscape and (version:6), ie and (version:7), ie and (version:8)";
    String anotherBrowserSelector = browserSelector;
    String anotherBrowserDiffOrder = "ie and (version:8), ie and (version:7), netscape and (version:6), netscape and (version:5)";

    int[] platforms = {2, 3, 4};
    int[] anotherPlatforms = {2, 3, 4};
    int[] differentOrderPlatforms = {2, 4, 3};

    Set<String> accProps = new HashSet<String>(Arrays.asList("high-contrast", "large-fonts"));
    Set<String> anotherAccProps = new HashSet<String>(Arrays.asList("high-contrast", "large-fonts"));
    Set<String> differentOrderAccProps = new HashSet<String>(Arrays.asList("large-fonts", "high-contrast"));


    // The constructor takes these arguments:
    // StyleNode[] styles,
    // Locale[] locales,
    // int direction,
    // int[] browsers,
    // int[] platforms,
    // int mode

    StyleSheetNode styleSheetNode =
      new StyleSheetNode(styleSheetOneNodes,
                         iconNodes,
                         localeSet,
                         0,
                         new AgentAtRuleMatcher(browserSelector),
                         platforms,
                         0,
                         accProps);
    StyleSheetNode anotherStyleSheetNode =
      new StyleSheetNode(anotherStyleSheetOneNodes,
                         anotherIconNodes,
                         localeSet,
                         0,
                         new AgentAtRuleMatcher(anotherBrowserSelector),
                         anotherPlatforms,
                         0,
                         anotherAccProps);
    StyleSheetNode sameDiffOrderStyleSheetNode =
      new StyleSheetNode(anotherStyleSheetOneNodes,
                         anotherIconNodes,
                         diffOrderLocalesSet,
                         0,
                         new AgentAtRuleMatcher(anotherBrowserDiffOrder),
                         differentOrderPlatforms,
                         0,
                         differentOrderAccProps);

    // these should be equal
    assertEquals(styleSheetNode.getStyleSheetId() == anotherStyleSheetNode.getStyleSheetId(), true);
    assertEquals(styleSheetNode.getStyleSheetId() == sameDiffOrderStyleSheetNode.getStyleSheetId(), true);

    // Note that StyleSheetNode.equals() explicitly uses the default
    // identity comparison since we never care about logical equivalence
    // of two StyleSheetNode instances.  The StyleSheetNode equality
    // tests below enforce that StyleSheetNode.equals() implements identity
    // rather that logical equality.
    assertEquals(styleSheetNode.equals(styleSheetNode), true);
    assertEquals(anotherStyleSheetNode.equals(styleSheetNode), false);
    assertEquals(styleSheetNode.equals(anotherStyleSheetNode), false);
    assertEquals(sameDiffOrderStyleSheetNode.equals(anotherStyleSheetNode), false);
    assertEquals(styleSheetNode.equals(sameDiffOrderStyleSheetNode), false);

    // these should be false
    assertEquals(styleSheetNode.equals(null), false);
    assertEquals(styleSheetNode.equals(localeSet), false);

    /* Test styleSheetNode's toString */
    /*
    System.out.println(sameLocaleDiffOrderStyleSheetNode.toString());
    */

  }

  // returns a StyleNode for "AFDefaultFont" name, null selector, resetProperties = false
  private StyleNode getAfDefaultFontStyleNode()
  {
    PropertyNode fontSize = new PropertyNode("font-size", "10pt");
    PropertyNode fontWeight = new PropertyNode("font-weight", "normal");
    PropertyNode[] defaultFontPropertyNodes = { fontSize, fontWeight };

    IncludeStyleNode[] defaultFontIncludeStyles =
      { new IncludeStyleNode("AFDefaultFontFamily", null) };

    IncludePropertyNode[] defaultFontIncludeProperty =
      { new IncludePropertyNode("AFVeryDarkBackground", null, "background-color", "color") };

    return
      new StyleNode("AFDefaultFont", null, defaultFontPropertyNodes, null,
                    defaultFontIncludeStyles, defaultFontIncludeProperty, null, null, false);

  }

  // returns a StyleNode for "AFDefaultFont" name, null selector, resetProperties = true
  private StyleNode getAfDefaultFontResetStyleNode()
  {
    PropertyNode fontSize = new PropertyNode("font-size", "10pt");
    PropertyNode fontWeight = new PropertyNode("font-weight", "normal");
    PropertyNode[] defaultFontPropertyNodes = { fontSize, fontWeight };

    IncludeStyleNode[] defaultFontIncludeStyles =
      { new IncludeStyleNode("AFDefaultFontFamily", null) };

    IncludePropertyNode[] defaultFontIncludeProperty =
      { new IncludePropertyNode("AFVeryDarkBackground", null, "background-color", "color") };

    return
      new StyleNode("AFDefaultFont", null, defaultFontPropertyNodes, null,
                    defaultFontIncludeStyles, defaultFontIncludeProperty, null, null, true);

  }

  // returns a StyleNode for "af|outputLabel:error" selector
  private StyleNode getOutputLabelStyleNode()
  {
    // build up another StyleNode
    IncludeStyleNode[] labelIncludeStyles = {new IncludeStyleNode("AFLabel", null)};
    PropertyNode[] labelPropertyNodes = {new PropertyNode("color", "red")};

    Set<String> labelInhibitedProperties = new HashSet<String>();
    labelInhibitedProperties.add("font-size");
    labelInhibitedProperties.add("background-color");


    return
        new StyleNode(null, "af|outputLabel:error", labelPropertyNodes, null, labelIncludeStyles,
                      null, null, labelInhibitedProperties, false);
  }

  // returns a StyleNode that matches the outputLabelStyleNode
  // the inhibited properties are added in a different order, that's all.
  private StyleNode getAnotherOutputLabelStyleNode()
  {
    IncludeStyleNode[] anotherIncludeStyles = {new IncludeStyleNode("AFLabel", null)};
    PropertyNode[] anotherPropertyNodes = {new PropertyNode("color", "red")};
    Set<String> anotherInhibitedProperties = new HashSet<String>();
    anotherInhibitedProperties.add("background-color");
    anotherInhibitedProperties.add("font-size");

    return  new StyleNode(null, "af|outputLabel:error", anotherPropertyNodes, null, anotherIncludeStyles,
                          null, null, anotherInhibitedProperties, false);
  }

  private Set<Locale> getLocalesSet()
  {
    Set<Locale> set = new HashSet<Locale>();
    set.add(new Locale("zh", "TW"));
    set.add(new Locale("zh", "CN"));
    return set;
  }

  private Set<Locale> getDiffOrderLocalesSet()
  {
    Set<Locale> set = new HashSet<Locale>();
    set.add(new Locale("zh", "CN"));
    set.add(new Locale("zh", "TW"));

    return set;
  }

  // same as above
  private Locale[] getLocalesArray()
  {
    return new Locale[] {new Locale("tw", "TW"), new Locale("zh", "CN")};
  }

  private Locale[] getAnotherLocalesArray()
  {
    return new Locale[] {new Locale("tw", "TW"), new Locale("zh", "CN")};
  }

  // same as above, different order
  private Locale[] getDiffOrderLocalesArray()
  {
    return new Locale[] {new Locale("zh", "CN"), new Locale("tw", "TW")};
  }

  private List<IconNode> _getIconNodes()
  {
    List<IconNode> iconNodes = new ArrayList<IconNode>(2);

    Icon icon1 = new TextIcon("Hello, world!");
    Icon icon2 = new ContextImageIcon("/foo/bar/baz.png", 10, 10);

    iconNodes.add(new IconNode("hello", icon1));
    iconNodes.add(new IconNode("foo", icon1));

    return iconNodes;
  }
}



