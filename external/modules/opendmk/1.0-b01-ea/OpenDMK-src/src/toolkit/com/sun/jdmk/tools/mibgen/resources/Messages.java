/*
 * @(#)file      Messages.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.45
 * @(#)date      07/04/04
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 *
 */


package com.sun.jdmk.tools.mibgen.resources;



import java.util.ListResourceBundle;
import com.sun.jdmk.tools.mibgen.MibGenProperties;

public class Messages extends ListResourceBundle {

    public Object[][] getContents() {
        return contents;
    }

    static final Object[][] contents = {

        {"compile.info.start", "Start compiling {0}"},
        {"compile.info.start.mibcore", "Start compiling default MIB-CORE definitions file: {0}"},
        {"compile.info.start.agent", "Start generating agent code for mib: {0}"},
        {"compile.info.start.oidtable", "Start generating metadata definitions code for variables of mib: {0}"},
        {"compile.info.endParse", "Has parsed module \"{0}\""},
        {"compile.nb.error", "{0} compilation error was encountered."},
        {"compile.nb.warning", "{0} warning was encountered."},
        {"compile.nb.errors", "{0} compilation errors were encountered."},
        {"compile.nb.warnings", "{0} warnings were encountered."},
        {"compile.resolve.local", "Start local resolution for module  {0}"},
        {"compile.resolve.global", "Start global resolution for module {0}"},
        {"compile.resolve.info", " \"{0}\" from mib {1} is resolved using definition from mib {2}"},
        {"compile.error", "Error: "},
        {"compile.error.internal.outmemory", "The compiler is running out of memory."},
        {"compile.error.internal", "an internal error of type \"{0}\" occured."},
        {"compile.error.option.invalid", "Invalid option: \"{0}\"."},
        {"compile.error.option.level", "{0} option requires an argument."},
        {"compile.error.option.incompat", "Specify only one of {0}/{1}."},
        {"compile.error.stop", "Compilation stopped"},
        {"compile.error.noFile", "Can not open file {0}"},
        {"compile.error.noMibCoreFile", "Can not open default MIB-CORE definitions file: {0}"},
        {"compile.error.noDir", "Can not open directory {0}. Invalid target directory."}, 
        {"compile.error.noWritePermission", "Can not write in {0}. Invalid target directory."},
        {"compile.error.duplicate.module" , "You have several MIB modules defined with name \"{0}\""},
        {"compile.error.duplicate.oid", "Symbols \"{0}\" and \"{1}\" have the same oid ({2}) in module {3}"},
        {"compile.error.undef" , "Symbol \"{0}\" in module \"{1}\" can not be resolved"},
        {"compile.error.loop" , "Loop detected with \"{0}\" when computing \"{1}\" in module \"{2}\""},
        {"compile.error.io" , "An IO error occured when compiling  \"{0}\""},
        {"compile.error.multiple.objectidentity", "Symbol OBJECT-IDENTITY \"{0}\" is multiple defined in module {1}"},
        {"compile.error.multiple.objectgroup", "Symbol OBJECT-GROUP \"{0}\" is multiple defined in module {1}"},
        {"compile.error.multiple.notifgroup", "Symbol NOTIFICATION-GROUP \"{0}\" is multiple defined in module {1}"},
        {"compile.error.multiple.notificationtype", "Symbol NOTIFICATION-TYPE \"{0}\" is multiple defined in module {1}"},
        {"compile.error.multiple.register", "Symbol \"{0}\" is multiple registered in module {1}"},
        {"compile.w.export", "Should not use clause EXPORTS line {0}"},
        {"compile.w.range", "Invalid range defined line {0}"},
        {"compile.w.resolve", "Multiple resolutions for symbol \"{0}\""},
        {"compile.w.value", "Invalid value ({0}) defined line {1}"},
        {"compile.w.enum", "Invalid value (0) for a enum defined line {0}"},
        {"compile.w.multiple.syntax", "Syntax symbol \"{0}\" is multiple defined in module {1}"},
        {"compile.w.multiple.value", "Symbol value \"{0}\" is multiple defined in module {1}"},
        {"compile.w.multiple.v1Object", "SNMPv1 Object type \"{0}\" is multiple defined in module {1}"},
        {"compile.w.multiple.v2Object", "Object type \"{0}\" is multiple defined in module {1}"},
        {"compile.w.defval" , "Invalid default value \"{0}\" for variable \"{1}\". Default initialization incomplete."},
        {"compile.warning", "Warning: "},
        {"parse.exception.lexical.err", "Lexical error at line {0}, column {1}. Encountered {2} after {3}"},
        {"parse.exception.msg.pos", "Encountered \"{0}\" at line {1}, column {2}"},
        {"parse.exception.msg.exp", "Was Expecting: {0}"},
        {"parse.exception.msg.exps", "Was Expecting one of: {0}"},
        {"usage.a", "Generate code for all the mib files;\n\t\tWithout this option, code is generated only for the first mib file;\n\t\tIn this case, the following files are simply used to resolve mib definitions."},
        {"usage.desc", "Include the \"DESCRIPTION\" clause of OBJECT-TYPE as comment in generated code."},
        {"usage.n", "Parse the mib files without generating code."},
        //{"usage.m", "Generate code for the manager API (in addition to the agent code);\n\t\tIncompatible with -n."},
        {"usage.mo", "Only generate code for the metadata definitions file for the MIB variables (SnmpOidTable file);\n\t\tIncompatible with -n."},
        {"usage.mc", "Specify to not use the default MIB-CORE definitions file provided with Java DMK.\n\t\tIn this case, the user must specify the MIB-CORE definitions file as one of the <mib files>."},
        {"usage.p", "Use the specified prefix for naming generated classes."},

	{"usage.x", "Specify a mibgen advanced option."+
	 "\n\t\tUse -X:help for more information."},
	{"usage.x.option","Available advanced options"},
	{"usage.x.define", "Define property." +
	 "\n\t\tDefines a valid mibgen property in the form <name>=<value>"},
	{"usage.x.abstract","Generate abstract MIB." +
         "\n\t\tWhen on, instruct mibgen to generate an abstract MIB."+
	 "\n\t\tThe MIB class will be an abstract class where the MBean "+
	 "\n\t\tfactory methods are abstract."+
	 "\n\t\t["+MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT+"=true]"},
	{"usage.x.display", "Use DISPLAY-HINT." +
         "\n\t\tWhen on, instruct mibgen to generate an attribute of "+
	 "\n\t\ttype String for any object using a textual convention"+
	 "\n\t\twhose DISPLAY-HINT is \"255a\""+
	 "\n\t\t["+MibGenProperties.OPTION_USE_DISPLAY_HINT+"=true]"},
	{"usage.x.table.noaccess", "No table accessor."+
	 "\n\t\tWhen on, instruct mibgen not to generate any table accessor"+
	 "\n\t\tin the group MBean interfaces."+
	 "\n\t\t["+MibGenProperties.OPTION_MBEAN_TABLE_ACCESSOR+"=false]"},
	{"usage.x.help", "Print this help message."},
	{"usage.x.target", "Generate mib compatible with a specific" +
	 "\n\t\tJava DMK release."+
	 "\n\t\t-X:target:5.0 generates a mib compatible with Java DMK 5.0"},
	{"usage.x.ulong", "Generates a mib that handles COUNTER64 as " + 
	 "\n\t\tan UnsignedLong."},
        {"usage.s", "Generate a stand-alone agent which will run without the MBean server."},
        {"usage.dir", "Generate code in the specified target directory."},
        {"usage.mib", "List of MIB files to compile."},
        {"usage.help", "Print this help message."},
        {"usage.tp", "Generate code within the specified Java package."},
        {"usage.where", "where <options> includes:"},
        {"usage.g", "Generates generic metadata that accesses MBeans through the MBeanServer."},
        {"usage.gp", "Uses the specified prefix string to name generic\n\t\tmetadata classes (only meaningful with -g option);\n\t\te.g. the Metadata class associated with the \"System\" group\n\t\twill be named System<GenericPrefix>Meta."},
        {"usage.sp", "Uses the specified prefix string to name standard\n\t\tmetadata classes (meaningless with -g option);\n\t\te.g. the Metadata class associated with the \"System\" group\n\t\twill be named System<StandardPrefix>Meta."},
        {"generate.enum.comment.desc", "The class is used for representing \"{0}\"."},
        {"generate.version", "Generated by mibgen version 5.1 (03/08/07) when compiling {0}."},
        {"generate.version.generic", "Generated by mibgen version 5.1 (03/08/07) when compiling {0} in generic metadata mode."},
        {"generate.version.standard", "Generated by mibgen version 5.1 (03/08/07) when compiling {0} in standard metadata mode."},
        {"generate.info.if", "Generating interface for \"{0}\"."},
        {"generate.info.var", "Generating code for \"{0}\"."},
        {"generate.info.meta", "Generating metadata code for \"{0}\"."},
        {"generate.info.enum", "Generating code for representing enumeration \"{0}\"."},
        {"generate.error.mib", "Invalid MIB structure: Group {0} contains another group {1}"},
        {"generate.error.subtype", "Subtyping of \"{0}\" is higher ({1}) than the defined type ({2})."},
        {"generate.error.table.index", "Can not find index \"{0}\" defined for table \"{1}\"."},
        {"generate.error.table.entry", "Several entry types are associated to table \"{0}\"."},
        {"generate.error.table.noIndex", "Could not find any index definition for table \"{0}\"."},
        {"generate.mbeanif.comment.desc", "This interface is used for representing the remote management interface for the \"{0}\" MBean."},
        {"generate.mbean.comment.constr", "Constructor for the \"{0}\" group."},
        {"generate.mbean.comment.noRegistration", "If the group contains a table, the entries created through an SNMP SET will not be registered in Java DMK."},
        {"generate.mbean.comment.registration", "If the group contains a table, the entries created through an SNMP SET will be AUTOMATICALLY REGISTERED in Java DMK."},
        {"generate.mbean.comment.desc", "The class is used for implementing the \"{0}\" group."},
        {"generate.mbean.comment.checker", "Checker for the \"{0}\" variable."},
        {"generate.mbean.comment.checker.policy", "Add your own checking policy."},
        {"generate.mbean.comment.checker.rs.deprecated", "@deprecated This method is never called."},
        {"generate.mbean.comment.checker.rs.override", "     Override checkRowStatusChange on SnmpMibTable if needed."},
        {"generate.mbean.comment.checker.rs.policy", "This method is generated for backward compatibility. "},
        {"generate.mbean.comment.table.access", "Access the \"{0}\" variable."},
        {"generate.mbean.comment.table.entry", "Access the \"{0}\" variable as a bean indexed property."},
        {"generate.mbean.comment.getter", "Getter for the \"{0}\" variable."},
        {"generate.mbean.comment.setter", "Setter for the \"{0}\" variable."},
	{"generate.mbean.comment.setter.rs.nochecker", "NB: There is no check method generated for RowStatus."},
        {"generate.mbean.comment.varUse", "Variable for storing the value of \"{0}\"."},
        {"generate.mbean.comment.varFix", "In the SNMP MIB, this is defined as a fixed length string of size {0}."},
        {"generate.mbean.comment.varOid", "The variable is identified by: \"{0}\"."},
        {"generate.mbean.comment.oid", "The group is defined with the following oid: {0}."},
        {"generate.meta.comment.constr", "Constructor for the metadata associated to \"{0}\"."},
        {"generate.meta.comment.create1", "The method allows the creation of an entry in the table through a SNMP SET."},
        {"generate.meta.comment.create2", "To enable such a feature, you need to make the current class derived from"},
        {"generate.meta.comment.create3", "the \"SnmpMibTableRemCreate\" class provided by the SNMP package."},
        {"generate.meta.comment.create4", "In the current class, replace the \"extends SnmpMibTable\" clause with "},
        {"generate.meta.comment.create5", "\"extends SnmpMibTableRemCreate\" to change the default behavior. "},
        {"generate.meta.comment.create6", "By default the toolkit does not allow creation of entries through a "},
        {"generate.meta.comment.create7", "management operation. As such the following method is not required."},
        {"generate.meta.comment.desc", "The class is used for representing SNMP metadata for the \"{0}\" group."},
        {"generate.meta.comment.checker", "Implement the \"check\" method from the abstract SnmpMibNode class."},
        {"generate.meta.comment.getter", "Implement the \"get\" method from the abstract SnmpMibNode class."},
        {"generate.meta.comment.getNext", "Implement the \"get next\" method from the abstract SnmpMibNode class."},
        {"generate.meta.comment.setter", "Implement the \"set\" method from the abstract SnmpMibNode class."},
        {"generate.meta.comment.deprecated", "@deprecated This method is no longer called."},
        {"generate.meta.comment.getvar", "Get the value of a scalar variable"},
        {"generate.meta.comment.setvar", "Set the value of a scalar variable"},
        {"generate.meta.comment.checkvar", "Check the value of a scalar variable"},
        //        {"generate.meta.comment.getrequest", "Implement the \"getEntryValues\" method from the abstract SnmpMibTable class."},
        //        {"generate.meta.comment.setrequest", "Implement the \"setEntryValues\" method from the abstract SnmpMibTable class."},
        //        {"generate.meta.comment.checkrequest", "Implement the \"checkEntryValues\" method from the abstract SnmpMibTable class."},
        {"generate.meta.comment.setMoi", "Allow to bind the metadata description to a specific object."},
        {"generate.meta.comment.isvariable", "Returns true if \"{0}\" identifies a scalar object."},
        {"generate.meta.comment.isreadable", "Returns true if \"{0}\" identifies a readable scalar object."},
        {"generate.meta.comment.istable", "Returns true if \"{0}\" identifies a table object."},
        {"generate.meta.comment.gettable", "Returns the table object identified by \"{0}\"."},
        {"generate.meta.comment.getnextvarid", "Returns the arc of the next columnar object following \"{0}\"."},
        {"generate.meta.comment.validatevarid", "check that the given \"{0}\" identifies a columnar object."},
        {"generate.meta.comment.table.index", "Build index for \"{0}\"."},
        {"generate.meta.comment.table.constr", "Constructor for the table. Initialize metadata for \"{0}\"."},
        {"generate.meta.comment.table.noRegistration", "The reference on the MBean server is not updated so the entries created through an SNMP SET will not be registered in Java DMK."},
        {"generate.meta.comment.table.registration", "The reference on the MBean server is updated so the entries created through an SNMP SET will be AUTOMATICALLY REGISTERED in Java DMK."},
        {"generate.meta.comment.table.var", "Reference to the entry metadata."},
        {"generate.meta.comment.table.server", "Reference to the MBean server."},
        {"generate.mib.comment.header", "The class is used for representing \"{0}\"."},
        {"generate.mib.comment.const", "Default constructor. Initialize the Mib tree."},
        {"generate.mib.comment.cmf", "Initialize MBeanServer information."},
        {"generate.mib.comment.modif", "You can edit the file if you want to modify the behavior of the MIB."},
        {"generate.mib.comment.import", "Dependency on Java Dynamic Management Kit."},
        {"generate.mib.comment.init", "Initialization of the MIB with no registration in Java DMK."},
        {"generate.mib.comment.preRegister", "Initialization of the MIB with AUTOMATIC REGISTRATION in Java DMK."},
        {"generate.mib.comment.oneCall", "Allow only one initialization of the MIB."},
        {"generate.mib.comment.init.group", "Initialization of the \"{0}\" group."},
        // XXX
        {"generate.table.comment.getentries","Return the entries stored in the table."},
        {"generate.table.comment.remove","Remove the specified entry from the table."},
        {"generate.table.comment.add1","Add a new entry to the table."},
        {"generate.table.comment.add2","If the associated metadata requires ObjectNames"},
        {"generate.table.comment.add3","a new ObjectName will be generated using \"{0}\"."},
        {"generate.table.comment.calls","This method calls \"{0}\" from \"{1}\"."},
        {"generate.meta.comment.setobjname", "Set the ObjectName of the MBean corresponding to this group/entry"},
        {"generate.meta.comment.checkset", "Check the access rights for a SET operation"},
        {"generate.meta.comment.checkget", "Check the access rights for a GET operation"},
        {"generate.meta.comment.buildatt", "Construct an Attribute value as returned by getValue() from an SnmpValue"},
        {"generate.meta.comment.buildval", "Construct an SnmpValue from an Attribute value returned by getValue()"},
        {"generate.meta.comment.getattname", "Return the name of the attribute corresponding to the SNMP variable identified by \"{0}\"."},
        {"generate.meta.comment.regtables", "Register the group's SnmpMibTable objects with the meta-data."},
        {"generate.meta.comment.table.objserver", "Reference to the object server."},
        {"generate.meta.comment.table.setobjsrv", "Allow to bind the metadata description to a specific object server."},
        {"generate.meta.comment.impl.proposed1", "A default implementation could be for instance:"},
        {"generate.meta.comment.impl.proposed2", "We know we won't need the OID."},
        {"generate.mib.comment.init.support1", "To disable support of this group, redefine the "},
        {"generate.mib.comment.init.support2", "\"{0}()\" factory method, and make it return \"null\""},

        {"generate.mib.comment.factory.abstract.meta", "Factory method for \"{0}\" group metadata class."},
        {"generate.mib.comment.factory.abstract.bean", "Factory method for \"{0}\" group MBean."},
        {"generate.mib.comment.factory.text1.meta", "You can redefine this method if you need to replace the default"},
        {"generate.mib.comment.factory.text1.bean", "You can redefine this method if you need to replace the default"},
        {"generate.mib.comment.factory.text2.meta", "generated metadata class with your own customized class."},
        {"generate.mib.comment.factory.text2.bean", "generated MBean class with your own customized class."},
        {"generate.mib.comment.factory.param.name", "@param groupName Name of the group (\"{0}\")"},
        {"generate.mib.comment.factory.param.oid", "@param groupOid  OID of this group"},
        {"generate.mib.comment.factory.param.objname", "@param groupObjname ObjectName for this group (may be null)"},
        {"generate.mib.comment.factory.param.server", "@param server    MBeanServer for this group (may be null)"},
        {"generate.mib.comment.factory.return1.meta", "@return An instance of the metadata class generated for the"},
        {"generate.mib.comment.factory.return1.bean", "@return An instance of the MBean class generated for the"},
        {"generate.mib.comment.factory.return2.meta", "        \"{0}\" group ({1})"},
        {"generate.mib.comment.factory.return2.bean", "        \"{0}\" group ({1})"},
        {"generate.mib.comment.factory.entry.abstract.meta", "Factory method for \"{0}\" entry metadata class."},

        {"generate.mib.comment.factory.entry.abstract.bean", "Factory method for \"{0}\" entry MBean class."},
        {"generate.mib.comment.factory.entry.return2.bean", "        \"{0}\" conceptual row."},

        {"generate.mib.comment.factory.entry.param.name", "@param snmpEntryName Name of the SNMP Entry object (conceptual row) (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.tablename", "@param tableName Name of the table in which the entries are registered (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.mib",  "@param mib The SnmpMib object in which this table is registered"},
        {"generate.mib.comment.factory.entry.param.server", "@param server MBeanServer for this table entries (may be null)"},
        {"generate.mib.comment.factory.entry.return2.meta", "        \"{0}\" conceptual row ({1})"},

        {"generate.mib.comment.factory.table.abstract.meta", "Factory method for \"{0}\" table metadata class."},
        {"generate.mib.comment.factory.table.param.tablename", "@param tableName Name of the table object (\"{0}\")"},
        {"generate.mib.comment.factory.table.param.groupname", "@param groupName Name of the group to which this table belong (\"{0}\")"},
        {"generate.mib.comment.factory.table.param.mib",  "@param mib The SnmpMib object in which this table is registered"},
        {"generate.mib.comment.factory.table.param.server", "@param server MBeanServer for this table (may be null)"},
        {"generate.mib.comment.factory.table.return2.meta", "        \"{0}\" table ({1})"},

        {"generate.mib.comment.factory.note.return3.std", "Note that when using standard metadata,"},
        {"generate.mib.comment.factory.note.return4.std", "the returned object must implement the \"{0}\""},
        {"generate.mib.comment.factory.note.return5.std", "interface."},
        {"generate.mib.comment.factory.note.return3.gen", "Note that when using generic metadata,"},
        {"generate.mib.comment.factory.note.return4.gen", "the returned object can either implement the \"{0}\""},
        {"generate.mib.comment.factory.note.return5.gen", "interface, or implement the \"DynamicMBean\" interface."},

        {"generate.mib.comment.implements", "Implements the \"{0}\" method defined in \"{1}\"."},
        {"generate.mib.comment.seedoc", "See the \"{0}\" Javadoc API for more details."},

        {"generate.miboidtable.comment.header", "The class contains metadata definitions for \"{0}\"."},
        {"generate.miboidtable.comment.header2", "Call SnmpOid.setSnmpOidTable(new {0}()) to load the metadata in the SnmpOidTable."},

    };
}
