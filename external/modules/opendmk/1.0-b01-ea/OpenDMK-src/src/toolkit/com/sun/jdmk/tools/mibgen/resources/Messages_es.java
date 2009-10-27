/*
 * @(#)file      Messages_es.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.52
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

public class Messages_es extends ListResourceBundle {

    public Object[][] getContents() {
        return contents;
    }

    static final Object[][] contents = {

        {"compile.info.start", "Compilando {0}"},
        {"compile.info.start.mibcore", "Compila el fichero de definiciones MIB-CORE por defecto: {0}"},
        {"compile.info.start.agent", "Genera el c\363digo de la parte agente de la mib: {0}"},
        {"compile.info.start.oidtable", "Genera el c\363digo de las definiciones de las variables de la mib: {0}"},
        {"compile.info.endParse", "Compilaci\363n del m\363dulo \"{0}\" finalizada."},
        {"compile.nb.error", "{0} error de compilaci\363n ha sido detectado."},
        {"compile.nb.warning", "{0} advertencia ha sido detectada."},
        {"compile.nb.errors", "{0} errores de compilaci\363n han sido detectados."},
        {"compile.nb.warnings", "{0} advertencias han sido detectadas."},
        {"compile.resolve.local", "Resoluci\363n local del m\363dulo {0}"},
        {"compile.resolve.global", "Resoluci\363n global del m\363dulo {0}"},
        {"compile.resolve.info", " S\355mbolo \"{0}\" referenciado en la mib {1} es resuelto en la mib {2}"},
        {"compile.error", "Error: "},
        {"compile.error.internal.outmemory", "El compilador no dispone de suficiente memoria."},
        {"compile.error.internal", "Un error interno de tipo \"{0}\" se ha producido."},
        {"compile.error.option.invalid", "La opci\363n \"{0}\" es inv\341lida."},
        {"compile.error.option.level", "La opci\363n \"{0}\" necesita un argumento."},
        {"compile.error.option.incompat", "Especificar solamente uno de {0}/{1}."},
        {"compile.error.stop", "Compilaci\363n abortada."},
        {"compile.error.noFile", "El fichero {0} no pudo ser abierto."},
        {"compile.error.noMibCoreFile", "No puede abrir el fichero de definiciones MIB-CORE por defecto: {0}"},
        {"compile.error.noDir", "El directorio {0} no pudo ser abierto."}, 
        {"compile.error.noWritePermission", "Operaci\363n de escritura en el directorio {0} no permitida."},
        {"compile.error.duplicate.module" , "Varios m\363dulos diferentes contienen el s\355mbolo \"{0}\"."},
        {"compile.error.duplicate.oid", "Los s\355mbolos \"{0}\" y \"{1}\" tienen el mismo OID ({2}) en el m\363dulo {3}"},
        {"compile.error.undef" , "El s\355mbolo \"{0}\" en el m\363dulo \"{1}\" no puede ser resuelto."},
        {"compile.error.loop" , "Un bucle con \"{0}\" ha sido detectado durante el tratamiento de \"{1}\" en el m\363dulo \"{2}\""},
        {"compile.error.io" , "Un error de E/S se ha producido durante la compilaci\363n de \"{0}\""},
        {"compile.error.multiple.objectidentity", "El objeto OBJECT-IDENTITY \"{0}\" ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.error.multiple.objectgroup", "El objeto OBJECT-GROUP \"{0}\" ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.error.multiple.notifgroup", "El objeto NOTIFICATION-GROUP \"{0}\" ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.error.multiple.notificationtype", "El objeto NOTIFICATION-TYPE \"{0}\" ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.error.multiple.register", "El s\355mbolo \"{0}\" ha sido registrado varias veces en el m\363dulo {1}"},
        {"compile.w.export", "La cl\341usula EXPORTS no deber\355a ser utilizada en la l\355nea {0}"},
        {"compile.w.range", "Rango inv\341lido definido en la l\355nea {0}"},
        {"compile.w.resolve", "Resoluciones m\372ltiples para el s\355mbolo \"{0}\""},
        {"compile.w.value", "Valor inv\341lido ({0}) definido en la l\355nea {1}"},
        {"compile.w.enum", "Valor inv\341lido (0) para una enumeraci\363n, definido en la l\355nea {0}"},
        {"compile.w.multiple.syntax", "La sintaxis \"{0}\" ha sido definida varias veces en el m\363dulo {1}"},
        {"compile.w.multiple.value", "El s\355mbolo \"{0}\" ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.w.multiple.v1Object", "El objeto \"{0}\", de tipo SNMPv1 ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.w.multiple.v2Object", "El objeto \"{0}\", de tipo SNMPv2 ha sido definido varias veces en el m\363dulo {1}"},
        {"compile.w.defval" , "Valor por defecto \"{0}\" es inv\341lido para la variable \"{1}\". Inicializaci\363n por defecto incompleta."},
        {"compile.warning", "Advertencia: "},
        {"parse.exception.lexical.err", "Error l\351xico en la l\355nea {0}, columna {1}. Expresi\363n {2} despu\351s de {3}"},
        {"parse.exception.msg.pos", "Detectado \"{0}\" en la l\355nea {1}, columna {2}"},
        {"parse.exception.msg.exp", "Se esperaba: {0}"},
        {"parse.exception.msg.exps", "Se esperaba una de las expresiones siguientes: {0}"},
        {"usage.a", "Genera c\363digo Java para todos los ficheros;\n\t\tSin esta opci\363n, el c\363digo es generado para el primer fichero solamente;\n\t\tEn este caso, los ficheros siguientes son simplemente utilizados para resolver las definiciones de la primera mib."},
        {"usage.desc", "Incluye la cl\341usula \"DESCRIPTION\" definida en OBJECT-TYPE como un comentario en el c\363digo generado."},
        {"usage.n", "Analiza los ficheros sin generar c\363digo."},

	{"usage.x", "Especifica una opci\363n avanzada." +
	 "\n\t\tUtilizar -X:help para m\341s informati\363n."},
	{"usage.x.option","Lista de opciones avanzadas"},
	{"usage.x.define", "Define una propiedad." +
	 "\n\t\tDefine una propiedad v\341lida para mibgen con la forma " +
	 "<name>=<value>"},
	{"usage.x.abstract","Genera una MIB abstracta." +
         "\n\t\tSi `on', ordena a mibgen que genere una MIB abstracta." +
	 "\n\t\tLa clase de la MIB ser\341 una clase abstracta cuyos " +
	 "\n\t\tm\351todos de construcci\363n de MBean ser\341n abstractos." +
	 "\n\t\t["+MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT+"=true]"},
	{"usage.x.display", "Usa DISPLAY-HINT." +
         "\n\t\tSi `on', ordena a mibgen que genere un atributo de tipo " +
	 "\n\t\tString para todo objeto que use una TEXTUAL-CONVENTION " +
	 "\n\t\tcuyo DISPLAY-HINT sea \"255a\"" +
	 "\n\t\t["+MibGenProperties.OPTION_USE_DISPLAY_HINT+"=true]"},
	{"usage.x.table.noaccess", "No hay accesor de tabla." +
	 "\n\t\tSi `on', ordena a mibgen que no genere ning\372n accesor de " +
	 "\n\t\ttabla para las interfaces MBean de los grupos." +
	 "\n\t\t["+MibGenProperties.OPTION_MBEAN_TABLE_ACCESSOR+"=false]"},
        {"usage.x.help", "Imprime el mensaje de ayuda."},
	{"usage.x.target", "Genera una mib compatible con una versi\363n espec\355fica" +
       "\n\t\tde Java DMK." +
       "\n\t\t-X:target:5.0 genera una mib compatible con Java DMK 5.0"},
	{"usage.x.ulong", "Genera una mib que soporta los COUNTER64 "+ 
         "\n\t\tcomo UnsignedLong."},
        //{"usage.m", "Genera c\363digo para la API manager (adem\341s del c\363digo para el agente);\n\t\tIncompatible con -n."},
        {"usage.mo", "Genera c\363digo \372nicamente para las definiciones de las variables de la MIB (fichero SnmpOidTable);\n\t\tIncompatible con -n."},
        {"usage.mc", "Especifica la no utilizaci\363n del fichero de definiciones MIB-CORE suministrado por defecto;\n\t\tEn este caso, el usuario debe especificar el fichero de definiciones MIB-CORE a utilizar."},
        {"usage.p", "Permite especificar un prefijo para nombrar las clases Java generadas."},
        {"usage.s", "Genera un agente stand-alone que no tiene necesidad de un MBean server Java DMK para funcionar."},
        {"usage.dir", "Genera c\363digo en el directorio destino especificado."},
        {"usage.mib", "Lista de ficheros MIB a compilar."},
        {"usage.help", "Imprime el mensaje de ayuda."},
        {"usage.tp", "Genera c\363digo en un paquete Java en particular."},
        {"usage.where", "donde <opciones> incluye:"},
        {"usage.g", "Genera las metaclases gen\351ricas que acceden a los MBeans a trav\351s del MBeanServer."},
        {"usage.gp", "Utiliza el prefijo dado para nombrar las metaclases\n\t\tgen\351ricas (solamente v\341lido con la opci\363n -g);\n\t\tEjemplo: la metaclase asociada al grupo \"System\"\n\t\tser\341 llamada System<GenericPrefix>Meta."},
        {"usage.sp", "Utiliza el prefijo dado para nombrar las metaclases\n\t\test\341ndar (no v\341lido con la opci\363n -g);\n\t\tEjemplo: la metaclase asociada al grupo \"System\"\n\t\tser\341 llamada System<StandardPrefix>Meta."},
        {"generate.enum.comment.desc", "La clase es utilizada para representar \"{0}\"."},
        {"generate.version", "Generado por mibgen version 5.1 (03/08/07) para compilar {0}."},
        {"generate.version.generic", "Generado por mibgen version 5.1 (03/08/07) para compilar {0} en modo metadata gen\351rico."},
        {"generate.version.standard", "Generado por mibgen version 5.1 (03/08/07) para compilar {0} en modo metadata est\341ndar."},
        {"generate.info.if", "Genera el c\363digo para la interfaz \"{0}\"."},
        {"generate.info.var", "Generando c\363digo para \"{0}\"."},
        {"generate.info.meta", "Generando metadata para \"{0}\"."},
        {"generate.info.enum", "Generando c\363digo para representar una enumeraci\363n \"{0}\"."},
        {"generate.error.mib", "Estructura de MIB incorrecta: El grupo {0} contiene al grupo {1}"},
        {"generate.error.subtype", "El subtipo \"{0}\" es m\341s grande ({1}) que su tipo de base ({2})."},
        {"generate.error.table.index", "La definici\363n del \355ndice \"{0}\" para la tabla \"{1}\" no ha sido encontrado."},
        {"generate.error.table.entry", "Tipos de entradas diferentes han sido asociados a la tabla \"{0}\"."},
        {"generate.error.table.noIndex", "La definici\363n del \355ndice para la tabla \"{0}\" no ha sido encontrado."},
        {"generate.mbeanif.comment.desc", "La interfaz es utilizada para representar la interfaz de administraci\363n remota del MBean \"{0}\"."},
        {"generate.mbean.comment.constr", "Constructor para el grupo \"{0}\"."},
        {"generate.mbean.comment.noRegistration", "Si el grupo contiene una tabla, las entradas creadas v\355a un SNMP SET no ser\341n registradas en Java DMK."},
        {"generate.mbean.comment.registration", "Si el grupo contiene una tabla, las entradas creadas v\355a un SNMP SET ser\341n AUTOMATICAMENTE registradas en Java DMK."},
        {"generate.mbean.comment.desc", "La clase es utilizada para implementar el grupo \"{0}\"."},
        {"generate.mbean.comment.checker", "Controlador para la variable \"{0}\"."},
        {"generate.mbean.comment.checker.policy", "A\361ada su propia polit\355ca de control."},
        {"generate.mbean.comment.checker.rs.deprecated", "@deprecated Este m\351todo no es llamado nunca."},
        {"generate.mbean.comment.checker.rs.override", "     Sustituir checkRowStatusChange en SnmpMibTable si es necesario."},
        {"generate.mbean.comment.checker.rs.policy", "Este m\351todo es generado para mantener la compatibilidad con versiones anteriores."},
        {"generate.mbean.comment.table.access", "Acceso a la variable \"{0}\"."},
        {"generate.mbean.comment.table.entry", "Acceso a la variable \"{0}\" como una propiedad indexada."},
        {"generate.mbean.comment.getter", "Getter para la variable \"{0}\"."},
        {"generate.mbean.comment.setter", "Setter para la variable \"{0}\"."},
	{"generate.mbean.comment.setter.rs.nochecker", "Nota: Ning\372n m\351todo check ser\341 generado para RowStatus."},
        {"generate.mbean.comment.varUse", "Variable para almacenar el valor de \"{0}\"."},
        {"generate.mbean.comment.varFix", "En la MIB, esta variable ha sido definida como un \"String\" de longitud fija igual a {0}."},
        {"generate.mbean.comment.varOid", "La variable es identificada por: \"{0}\"."},
        {"generate.mbean.comment.oid", "El grupo ha sido definido con el siguiente OID: {0}."},
        {"generate.meta.comment.constr", "Constructor para la metadata asociada a \"{0}\"."},
        {"generate.meta.comment.create1", "El m\351todo permite la creaci\363n de una entrada en la tabla a trav\351s de un SNMP SET."},
        {"generate.meta.comment.create2", "Para habilitar dicho comportamiento, la herencia de esta clase debe ser cambiada."},
        {"generate.meta.comment.create3", "Ella debe derivar de la clase \"SnmpMibTableRemCreate\" proporcionada por el paquete SNMP."},
        {"generate.meta.comment.create4", "En esta clase, reemplazar la cl\341usula \"extends SnmpMibTable\" por "},
        {"generate.meta.comment.create5", "\"extends SnmpMibTableRemCreate\" para cambiar el comportamiento por defecto. "},
        {"generate.meta.comment.create6", "Por defecto, el toolkit no permite la creaci\363n de una entrada en una tabla a trav\351s de "},
        {"generate.meta.comment.create7", "una operaci\363n de management. Por lo tanto, el m\351todo siguiente no es necesario."},
        {"generate.meta.comment.desc", "La clase es utlizada para representar la metadata asociada al grupo \"{0}\"."},
        {"generate.meta.comment.checker", "Implementa el m\351todo \"check\" derivado de la clase abstracta SnmpMibNode."},
        {"generate.meta.comment.getter", "Implementa el m\351todo \"get\" derivado de la clase abstracta SnmpMibNode."},
        {"generate.meta.comment.getNext", "Implementa el m\351todo \"get next\" derivado de la clase abstracta SnmpMibNode."},
        {"generate.meta.comment.setter", "Implementa el m\351todo \"set\" derivado de la clase abstracta SnmpMibNode."},
        {"generate.meta.comment.setMoi", "Permite asociar la metadata a un objeto espec\355fico."},
        {"generate.meta.comment.deprecated", "@deprecated Este m\351todo ya no ser\341 llamado m\341s."},
        {"generate.meta.comment.getvar", "Devuelve el valor de una variable escalar"},
        {"generate.meta.comment.setvar", "Actualiza el valor de una variable escalar"},
        {"generate.meta.comment.checkvar", "Comprueba el valor de una variable escalar"},
        //        {"generate.meta.comment.getrequest", "Implementa el m\351todo \"getEntryValues\" derivado de la clase abstracta SnmpMibTable."},
        //        {"generate.meta.comment.setrequest", "Implementa el m\351todo \"setEntryValues\" derivado de la clase abstracta SnmpMibTable."},
        //        {"generate.meta.comment.checkrequest", "Implementa el m\351todo \"checkEntryValues\" derivado de la clase abstracta SnmpMibTable."},
        {"generate.meta.comment.isvariable", "Devuelve true si \"{0}\" identifica a un objeto escalar."},
        {"generate.meta.comment.readable", "Devuelve true si \"{0}\" identifica a un objeto escalar accesible para lectura."},
        {"generate.meta.comment.istable", "Devuelve true si \"{0}\" identifica a una tabla."},
        {"generate.meta.comment.gettable", "Devuelve la tabla identificada por \"{0}\"."},
        {"generate.meta.comment.getnextvarid", "Devuelve el arco del objeto columna que sigue a \"{0}\"."},
        {"generate.meta.comment.validatevarid", "Comprueba que \"{0}\" identifica a un objeto columna."},
        {"generate.meta.comment.table.index", "Construyendo \355ndice para \"{0}\""},
        {"generate.meta.comment.table.constr", "Constructor para la tabla. Inicialice metadata para \"{0}\"."},
        {"generate.meta.comment.table.noRegistration", "La referencia del MBeanServer no es actualizada y las entradas creadas v\355a un SNMP SET no ser\341n registradas en Java DMK."},
        {"generate.meta.comment.table.registration", "La referencia del MBeanServer es actualizada y las entradas creadas v\355a un SNMP SET ser\341n AUTOMATICAMENTE registradas en Java DMK."},
        {"generate.meta.comment.table.var", "Referencia sobre la metadata de la entrada."},
        {"generate.meta.comment.table.server", "Referencia sobre el MBeanServer."},
        {"generate.mib.comment.header", "La clase es utilizada para representar \"{0}\"."},
        {"generate.mib.comment.const", "Constructor por defecto. Inicialice el \341rbol que representa la MIB."},
        {"generate.mib.comment.cmf", "Inicialice la informaci\363n referente al MBean server."},
        {"generate.mib.comment.modif", "Edite el fichero si quiere modificar el comportamiento de la MIB."},
        {"generate.mib.comment.import", "Dependencia sobre el producto Java Dynamic Management Kit."},
        {"generate.mib.comment.init", "Inicializaci\363n de la MIB sin registrarla en el MBean server Java DMK."},
        {"generate.mib.comment.preRegister", "Inicializaci\363n de la MIB con REGISTRO AUTOMATICO en el MBean server Java DMK."},
        {"generate.mib.comment.oneCall", "Solamente una inicializaci\363n de la MIB es autorizada."},
        {"generate.mib.comment.init.group", "Inicializaci\363n del grupo \"{0}\"."},
        {"generate.table.comment.getentries","Devuelve las entradas almacenadas en la tabla."},
        {"generate.table.comment.remove","Suprime la entrada especificada de la tabla."},
        {"generate.table.comment.add1","A\361ade una nueva entrada a la tabla."},
        {"generate.table.comment.add2","Si la metadata asociada necesita un ObjectName"},
        {"generate.table.comment.add3","un nuevo ObjectName ser\341 generado usando \"{0}\"."},
        {"generate.table.comment.calls","Este m\351todo llama al m\351todo \"{0}\" de \"{1}\"."},
        {"generate.mib.comment.init.support", "Para suprimir el soporte de este grupo, comente la secci\363n siguiente."},

        {"generate.meta.comment.setobjname", "Declara el ObjectName del MBean correspondiente a este grupo/entrada."},
        {"generate.meta.comment.checkset", "Comprueba los derechos de acceso para una operaci\363n SET"},
        {"generate.meta.comment.checkget", "Comprueba los derechos de acceso para una operaci\363n GET"},
        {"generate.meta.comment.buildatt", "Construye un valor de atributo como el devuelto por getValue(), a partir de un SnmpValue"},
        {"generate.meta.comment.buildval", "Construye un SnmpValue a partir del valor de un objeto Attribute devuelto por getValue()"},
        {"generate.meta.comment.getattname", "Devuelve el nombre del atributo correspondiente a la variable SNMP identificada por \"{0}\"."},
        {"generate.meta.comment.regtables", "Registra los objetos SnmpMibTable perteneciendo al grupo en el objeto Meta correspondiente."},
        {"generate.meta.comment.table.objserver", "Referencia sobre el servidor de objetos SNMP."},
        {"generate.meta.comment.table.setobjsrv", "Permite relacionar un objeto Meta con un servidor de objetos SNMP."},
        {"generate.meta.comment.impl.proposed1", "Una implementaci\363n por defecto podr\355a ser por ejemplo:"},
        {"generate.meta.comment.impl.proposed2", "Sabemos que el uso del OID no ser\341 necesario."},
        {"generate.mib.comment.init.support1", "Para suprimir el soporte de este grupo, redefina el m\351todo "},
        {"generate.mib.comment.init.support2", "\"{0}()\" de la factor\355a y haga que devuelva el valor \"null\""},

        {"generate.mib.comment.factory.abstract.meta", "M\351todo de construcci\363n para la meta-clase del grupo \"{0}\"."},
        {"generate.mib.comment.factory.abstract.bean", "M\351todo de construcci\363n para el MBean representando al grupo \"{0}\"."},
        {"generate.mib.comment.factory.text1.meta", "Puede redefinir este m\351todo si necesita reemplazarlo"},
        {"generate.mib.comment.factory.text1.bean", "Puede redefinir este m\351todo si necesita reemplazarlo"},
        {"generate.mib.comment.factory.text2.meta", "la meta-clase generada por defecto por su propia clase modificada."},
        {"generate.mib.comment.factory.text2.bean", "la clase del MBean generada por defecto por su propia clase modificada."},
        {"generate.mib.comment.factory.param.name", "@param groupName Nombre del grupo (\"{0}\")"},
        {"generate.mib.comment.factory.param.oid", "@param groupOid  OID del grupo"},
        {"generate.mib.comment.factory.param.objname", "@param groupObjname ObjectName de este grupo (puede ser nulo)"},
        {"generate.mib.comment.factory.param.server", "@param server    MBeanServer para este grupo (puede ser nulo)"},
        {"generate.mib.comment.factory.return1.meta", "@return Una instancia de la meta-clase generada para"},
        {"generate.mib.comment.factory.return1.bean", "@return Una instancia de la clase del MBean generada para"},
        {"generate.mib.comment.factory.return2.meta", "        el grupo \"{0}\" ({1})"},
        {"generate.mib.comment.factory.return2.bean", "        el grupo \"{0}\" ({1})"},

        {"generate.mib.comment.factory.entry.abstract.meta", "M\351todo de construcci\363n para la meta-clase de las entrads de la tabla \"{0}\"."},
        {"generate.mib.comment.factory.entry.abstract.bean", "M\351todo de construcci\363n para los MBeans representando a las entradas de la tabla \"{0}\"."},
        {"generate.mib.comment.factory.entry.param.name", "@param snmpEntryName Nombre del objeto SNMP Entry (conceptual row) (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.tablename", "@param tableName Nombre de la tabla donde las entradas son registradas (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.mib", "@param mib El objeto SnmpMib donde la tabla es registrada."},
        {"generate.mib.comment.factory.entry.param.server", "@param server El MBeanServer para las entradas de esta tabla (puede ser nulo)"},
        {"generate.mib.comment.factory.entry.return2.meta", "        las tuplas \"{0}\" (conceptual row) ({1})"},
        {"generate.mib.comment.factory.entry.return2.bean", "        las tuplas \"{0}\" (conceptual row)."},

        {"generate.mib.comment.factory.table.abstract.meta", "M\351todo de construcci\363n para la meta-clase de la tabla \"{0}\"."},
        {"generate.mib.comment.factory.table.param.tablename", "@param tableName Nombre de la tabla SNMP (\"{0}\")"},
        {"generate.mib.comment.factory.table.param.groupname", "@param groupName Nombre del grupo al que pertenece esta tabla (\"{0}\")"},
        {"generate.mib.comment.factory.table.param.mib",  "@param mib El objeto SnmpMib en el que la tabla est\341 registrada."},
        {"generate.mib.comment.factory.table.param.server", "@param server El MBeanServer de esta tabla (puede ser nulo)"},
        {"generate.mib.comment.factory.table.return2.meta", "        el grupo \"{0}\" ({1})"},

        {"generate.mib.comment.factory.note.return3.std", "Tenga en cuenta que cuando use la versi\363n est\341ndar"},
        {"generate.mib.comment.factory.note.return4.std", "de la metadata, el objeto devuelto debe implementar"},
        {"generate.mib.comment.factory.note.return5.std", "la interfaz \"{0}\"."},
        {"generate.mib.comment.factory.note.return3.gen", "Tenga en cuenta que cuando use la versi\363n gen\351rica"},
        {"generate.mib.comment.factory.note.return4.gen", "de la metadata, el objeto devuelto puede implementar la"},
        {"generate.mib.comment.factory.note.return5.gen", "interfaz \"{0}\" o la interfaz \"DynamicMBean\"."},

        {"generate.mib.comment.implements", "Implementa el m\351todo \"{0}\" definido en \"{1}\"."},
        {"generate.mib.comment.seedoc", "Mirar la API Javadoc de \"{0}\" para obtener un mayor detalle."},


        {"generate.miboidtable.comment.header", "La clase contiene las definiciones metadata para \"{0}\"."},
        {"generate.miboidtable.comment.header2", "Invoque SnmpOid.setSnmpOidTable(new {0}()) para cargar la metadata en el MibStore."},
    };

}
