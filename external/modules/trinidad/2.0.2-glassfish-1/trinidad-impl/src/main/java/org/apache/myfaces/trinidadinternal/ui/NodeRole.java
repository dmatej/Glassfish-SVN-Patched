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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * A NodeRole defines the role a single UINode plays in the rendering
 * process.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/NodeRole.java#0 $) $Date: 10-nov-2005.18:50:14 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class NodeRole
{
  /**
   * Creates a named NodeRole.
   * @param name an optional name;  NodeRole names do not need
   *  to be unique, and are used only for debugging.
   */
  public NodeRole(String name)
  {
    this(name, null);
  }


  /**
   * Creates a named NodeRole.
   * @param name an optional name;  NodeRole names do not need
   *  to be unique, and are used only for debugging.
   * @param roles a (possibly empty or null) list of roles
   *  that this role satisfies.
   */
  public NodeRole(String name, NodeRole[] roles)
  {
    _name = name;
    _base = false;
    if (roles != null)
    {
      int roleCount = 0;
      for (int i = 0; i < roles.length; i++)
      {
        NodeRole role = roles[i];
        _bits = _bits | role._bits;

        if (!role._base)
          roleCount++;
      }

      if (roleCount > 0)
      {
        _roles = new NodeRole[roleCount];
        for (int i = 0, j = 0; i < roles.length; i++)
        {
          NodeRole role = roles[i];
          if (!role._base)
          {
            _roles[j++] = role;
          }
        }
      }
    }
  }


  /**
   * Returns true if this role object "satisfies" another role.
   * This is true if and only if the roles are the same or
   * this role contains a NodeRole that satisfies the role.
   */
  public boolean satisfiesRole(NodeRole role)
  {
    if (role == this)
      return true;

    if (role._base)
      return ((_bits & role._bits) != 0);
    
    NodeRole[] roles = _roles;
    if (roles != null)
    {
      for (int i = 0; i < roles.length; i++)
      {
        if (roles[i].satisfiesRole(role))
          return true;
      }
    }

    return false;
  }

  /**
   * Returns a String useful for debugging.
   */
  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer();
    if (_name == null)
      buffer.append("NodeRole[anonymous");
    else
      buffer.append("NodeRole[name=" + _name);

    if (_roles != null)
    {
      for (int i = 0; i < _roles.length; i++)
      {
        buffer.append(',');
        buffer.append(_roles[i]);
      }
    }

    buffer.append(']');
    return buffer.toString();
  }

  //
  // Constructor for "base" NodeRoles.  "base" NodeRoles aren't
  // really any different from other roles - they just are a little
  // faster to find.
  //
  // =-=AEW In the future, we may very well wish to make this
  // public, just so we can create HTML roles (e.g., <table>
  // <tr>, <td>) or BLAF roles with indices and not need to
  // create those constants in this file.  It would still be
  // strictly restricted to Cabo to create these roles, and
  // we'd want to "pre-reserve" any unused indices by
  // initializing _sBits to something other than zero
  //
  NodeRole(String name, int roleIndex)
  {
    if (roleIndex >= 32)
      throw new IllegalArgumentException();
    
    _name = name;
    _base = true;
    _bits = 1 << roleIndex;

    // Make sure we don't reuse any indices
    synchronized(NodeRole.class)
    {
      if ((_sBits & _bits) != 0)
        throw new IllegalArgumentException(_LOG.getMessage(
          "REUSING_ROLE_INDEX"));
      _sBits = _sBits | _bits;
    }
  }

  private String     _name;
  private NodeRole[] _roles;
  private int        _bits;
  private boolean    _base;

  static private int _sBits = 0;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    NodeRole.class);
}
