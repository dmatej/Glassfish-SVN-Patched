/*
 * @(#)file      Messages_fr.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.43
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


// ATTENTION !!!  WARNING !!!!!! 
// AJOUT D'UN COMMENTAIRE CONTENANT UNE APOSTROPHE "'":
// si la phrase ne contient pas de reference {X}
//      => les apostrophes de cette phrase sont de simples apostrophes "'"
// si la phrase contient une reference {X}
//      => TOUTES les apostrophes de cette phrase sont des apostrophes doubles "''"




import com.sun.jdmk.tools.mibgen.MibGenProperties;
import java.util.ListResourceBundle;


public class Messages_fr extends ListResourceBundle {

    public Object[][] getContents() {
        return contents;
    }


    static final Object[][] contents = {

        {"compile.info.start", "Compile le module {0}"},
        {"compile.info.start.mibcore", "Compile le fichier des d\351finitions MIB-CORE fourni par d\351faut: {0}"},
        {"compile.info.start.agent", "G\351n\350re le code de la partie agent de la mib: {0}"},
        {"compile.info.start.oidtable", "G\351n\350re le code des d\351finitions des variables de la mib: {0}"},
        {"compile.info.endParse", "Compilation du module \"{0}\" termin\351e."},
        {"compile.nb.error", "{0} erreur de compilation a \351t\351 trouv\351e."},
        {"compile.nb.warning", "{0} avertissement a \351t\351 detect\351e."},
        {"compile.nb.errors", "{0} erreurs de compilation ont \351t\351 trouv\351es."},
        {"compile.nb.warnings", "{0} avertissements ont \351t\351 d\351tect\351s."},
        {"compile.resolve.local", "R\351solution locale du module {0}"},
        {"compile.resolve.global", "R\351solution globale du module {0}"},
        {"compile.resolve.info", "Symbole \"{0}\" r\351ferenc\351 dans la mib {1} est r\351solu dans la mib {2}"},
        {"compile.error", "Erreur: "},
        {"compile.error.internal.outmemory", "Le compilateur ne dispose plus d'assez de m\351moire."},
        {"compile.error.internal", "Un erreur interne de type \"{0}\" s''est produite."},
        {"compile.error.option.invalid", "L''option \"{0}\" est invalide."},
        {"compile.error.option.level", "L''option \"{0}\" n\351cessite un param\350tre."},
        {"compile.error.option.incompat", "Ne sp\351cifiez qu''une des options suivantes: {0}/{1}."},
        {"compile.error.stop", "Compilation arr\352t\351e."},
        {"compile.error.noFile", "Ne peut pas ouvrir le fichier \"{0}\""},
        {"compile.error.noMibCoreFile", "Ne peut pas ouvrir le fichier des d\351finitions MIB-CORE fourni par d\351faut: {0}"},
        {"compile.error.noDir", "Ne peut pas ouvrir le r\351pertoire cible {0}. "}, 
        {"compile.error.noWritePermission", "Ne peut pas \351crire dans le r\351pertoire cible {0}."},
        {"compile.error.duplicate.module" , "Plusieurs modules diff\351rents contiennent le symbole \"{0}\""},
        {"compile.error.duplicate.oid", "Les symboles \"{0}\" et \"{1}\" ont le m\352me OID ({2}) dans le module {3}"},
        {"compile.error.undef" , "Le symbole \"{0}\" dans le module \"{1}\" ne peut-\352tre r\351solu."},
        {"compile.error.loop" , "Une boucle avec \"{0}\" a \351t\351 d\351tect\351e pendant le traitement de \"{1}\" dans le module \"{2}\""},
        {"compile.error.io" , "Une IO erreur s''est produite lors de la compilation de  \"{0}\""},
        {"compile.error.multiple.objectidentity", "Le symbole OBJECT-IDENTITY \"{0}\" est d\351fini plusieurs fois dans le module {1}"},
        {"compile.error.multiple.objectgroup", "Le symbole OBJECT-GROUP \"{0}\" est d\351fini plusieurs fois dans le module {1}"},
        {"compile.error.multiple.notifgroup", "Le symbole NOTIFICATION-GROUP \"{0}\" est d\351fini plusieurs fois dans le module {1}"},
        {"compile.error.multiple.notificationtype", "Le symbole NOTIFICATION-TYPE \"{0}\" est d\351fini plusieurs fois dans le module {1}"},
        {"compile.error.multiple.register", "Le symbole \"{0}\"  est enregistr\351 plusieurs fois dans l''arbre d''OID du module {1}"},
        {"compile.w.export", "Ne devrait pas utiliser la clause EXPORTS \340 la ligne {0}"},
        {"compile.w.range", "Bornes invalides d\351finies \340 la ligne {0}"},
        {"compile.w.resolve", "R\351solutions multiples pour le symbole \"{0}\""},
        {"compile.w.value", "Valeur invalide ({0}) d\351finie \340 la ligne {1}"},
        {"compile.w.enum", "Valeur invalide (0) pour une \351num\351ration, d\351finie \340 la ligne {0}"},
        {"compile.w.multiple.syntax", "La syntaxe \"{0}\" est d\351finie plusieurs fois dans le module {1}"},
        {"compile.w.multiple.value", "Le symbole \"{0}\" est d\351fini plusieurs fois dans le module {1}"},
        {"compile.w.multiple.v1Object", "L''objet \"{0}\" , de type  SNMPv1 est d\351fini plusieurs fois dans le module {1}"},
        {"compile.w.multiple.v2Object", "L''objet \"{0}\" , de type  SNMPv2 est d\351fini plusieurs fois dans le module {1}"},
        {"compile.w.defval" , "Valeur par d\351fault \"{0}\" invalide pour la variable \"{1}\". Initialisation par d\351fault incompl\350te."},
        {"compile.warning", "Avertissement: "},
        {"parse.exception.lexical.err", "Erreur lexicale \340 la ligne {0}, colonne {1}. Expression {2} apr\350s {3}"},
        {"parse.exception.msg.pos", "Trouv\351 \"{0}\" \340 la ligne {1}, colonne {2}"},
        {"parse.exception.msg.exp", "Attend: {0}"},
        {"parse.exception.msg.exps", "Attend une des expressions suivantes: {0}"},
        {"usage.a", "G\351n\350re du code Java pour tous les fichiers;\n\t\tSans cette option, le code est g\351n\351 pour le premier fichier uniquement;\n\t\tDans ce cas, les fichiers suivants sont simplement utilis\351s pour r\351soudre les d\351finitions de la premi\350re mib."},
        {"usage.desc", "Int\350gre la clause \"DESCRIPTION\" d\351finie dans l'OBJECT-TYPE comme commentaire dans le code g\351n\351r\351."},
        {"usage.n", "Parse les fichiers sans g\351n\351rer de code."},

	{"usage.x", "Sp\351cifie une option avanc\351e."+
	 "\n\t\tUtiliser -X:help pour plus d'information."},
	{"usage.x.option","Liste des option avanc\351es"},
	{"usage.x.define", "D\351finit une propri\351t\351."+
	 "\n\t\tD\351finit une propri\351t\351 valide pour mibgen sous la "+
	 "\n\t\tforme <property-name>=<property-value>"},
	{"usage.x.abstract","G\351n\350re une MIB abstraite."+
         "\n\t\tSi `on', ordonne \340 mibgen de g\351n\351rer une MIB"+
	 "\n\t\tabstraite. La classe de la MIB sera une classe abstraite dont"+
	 "\n\t\tles m\351thodes de construction de MBean sont abstraites."+
	 "\n\t\t["+MibGenProperties.OPTION_MIB_FACTORY_ABSTRACT+"=true]"},
	{"usage.x.display", "Utilise DISPLAY-HINT."+
         "\n\t\tSi `on', ordonne \340 mibgen de g\351n\351rer un attribut de "+
	 "\n\t\ttype String pour tout object utilisant une TEXTUAL-CONVENTION"+
	 "\n\t\tdont le DISPLAY-HINT est \"255a\""+
	 "\n\t\t["+MibGenProperties.OPTION_USE_DISPLAY_HINT+"=true]"},
	{"usage.x.table.noaccess","Pas d'accesseur de table."+
	 "\n\t\tSi `on', ordonne \340 mibgen de ne g\351n\351rer aucun"+
	 "\n\t\taccesseur de table dans les interfaces MBean des groupes."+
	 "\n\t\t["+MibGenProperties.OPTION_MBEAN_TABLE_ACCESSOR+"=false]"},
        {"usage.x.help", "Affiche ce message d'aide."},
	{"usage.x.target", "G\351n\350re une MIB compatible avec une version" +
	 "\n\t\tsp\351cifique de Java DMK."+
	 "\n\t\t-X:target:5.0 G\351n\350re une MIB compatible Java DMK 5.0"},
	{"usage.x.ulong", "G\351n\350re une MIB qui g\350re les COUNTER64 "+
         "\n\t\t comme des UnsignedLong."},
        //{"usage.m", "G\351n\350re du code pour l'API manager (en plus du code agent);\n\t\tIncompatible avec -n."},
        {"usage.mo", "G\351n\350re du code uniquement pour les d\351finitions des variables MIB (fichier SnmpOidTable);\n\t\tIncompatible avec -n."},
        {"usage.mc", "N'utilise pas le fichier des d\351finitions MIB-CORE fourni par d\351faut;\n\t\tDans ce cas, l'utilisateur doit sp\351cifier le fichier des d\351finitions MIB-CORE \340 utiliser."},
        {"usage.p", "Permet de sp\351cifier un pr\351fixe pour nommer les classes Java g\351n\351r\351es."},
        {"usage.s", "G\351n\350re un agent stand-alone capable de tourner en dehors du MBean server Java DMK."},
        {"usage.dir", "G\351n\350re le code dans un r\351pertoire cible sp\351cifique."},
        {"usage.mib", "Liste des fichiers \340 compiler."},
        {"usage.help", "Affiche ce message d'aide."},
        {"usage.tp", "G\351n\350re le code dans un package Java particulier."},
        {"usage.where", "ou <options> comprend:"},
        {"usage.g", "G\351n\350re des meta classes g\351n\351riques acc\351dant aux MBeans au travers du MBeanServer."},
        {"usage.gp", "Utilise le pr\351fixe donn\351 pour nommer les meta \n\t\tclasses g\351n\351riques (seulement valide avec l'option -g);\n\t\tExample: la meta classe associ\351e au groupe \"System\"\n\t\ts'appellera System<GenericPrefix>Meta."},
        {"usage.sp", "Utilise le pr\351fixe donn\351 pour nommer les meta \n\t\tclasses standard (non valide avec l'option -g);\n\t\tExample: la meta classe associ\351e au groupe \"System\"\n\t\ts'appellera System<StandardPrefix>Meta."},
        {"generate.enum.comment.desc", "La classe est utilis\351e pour repr\351senter \"{0}\"."},
        {"generate.version", "G\351n\351r\351 avec mibgen version 5.1 (03/08/07) pour compiler {0}."},
        {"generate.version.generic", "G\351n\351r\351 avec mibgen version 5.1 (03/08/07) pour compiler {0} en mode metadata g\351n\351rique."},
        {"generate.version.standard", "G\351n\351r\351 avec mibgen version 5.1 (03/08/07) pour compiler {0} en mode metadata standard."},
        {"generate.info.if", "G\351n\350re le code pour l''interface \"{0}\"."},
        {"generate.info.var", "G\351n\350re le code pour \"{0}\"."},
        {"generate.info.meta", "G\351n\350re le code metadata pour \"{0}\"."},
        {"generate.info.enum", "G\351n\350re le code pour repr\351senter l''\351num\351ration \"{0}\"."},
        {"generate.error.mib", "Structure de MIB incorrecte:  Le groupe {0} contient un autre groupe {1}"},
        {"generate.error.subtype", "Le sous type \"{0}\" est plus grand ({1}) que son type de base ({2})."},
        {"generate.error.table.index", "Ne peut trouver la d\351finition de l''index \"{0}\" pour la table \"{1}\"."},
        {"generate.error.table.entry", "Diff\351rents types d''entr\351es sont associ\351es \340 la table \"{0}\"."},
        {"generate.error.table.noIndex", "Ne peut pas trouver de d\351finition d''index pour la table \"{0}\"."},
        {"generate.mbeanif.comment.desc", "L''interface est utilis\351e pour repr\351senter l''interface de management du MBean \"{0}\"."},
        {"generate.mbean.comment.constr", "Constructeur pour le groupe \"{0}\"."},
        {"generate.mbean.comment.noRegistration", "Si le groupe contient une table, les entr\351es cr\351\351es via un SNMP SET ne seront pas enregistr\351es dans Java DMK."},
        {"generate.mbean.comment.registration", "Si le groupe contient une table, les entr\351es cr\351\351es via un SNMP SET seront AUTOMATIQUEMENT enregistr\351es dans Java DMK."},
        {"generate.mbean.comment.desc", "La classe est utilis\351e pour implementer le groupe \"{0}\"."},
        {"generate.mbean.comment.checker", "Controleur pour la variable \"{0}\"."},
        {"generate.mbean.comment.checker.policy", "Ajoutez votre propre politique de contr\364le."},
        {"generate.mbean.comment.checker.rs.deprecated", "@deprecated Cette m\351thode n'est jamais appel\351e."},
        {"generate.mbean.comment.checker.rs.override", "     Redefinir checkRowStatusChange sur SnmpMibTable si n\351cessaire."},
        {"generate.mbean.comment.checker.rs.policy", "Cette m\351thode est g\351n\351r\351e pour assurer la compatibilit\351 arri\350re."},
        {"generate.mbean.comment.table.access", "Acc\351s \340 la variable \"{0}\"."},
        {"generate.mbean.comment.table.entry", "Acc\351s \340 la variable \"{0}\" comme une propri\351t\351 index\351e."},
        {"generate.mbean.comment.getter", "Getter pour la variable \"{0}\"."},
        {"generate.mbean.comment.setter", "Setter pour la variable \"{0}\"."},
	{"generate.mbean.comment.setter.rs.nochecker", "NB: Il n'y a pas de m\351thode check g\351n\351r\351e pour RowStatus."},
        {"generate.mbean.comment.varUse", "Variable pour stocker la valeur de \"{0}\"."},
        {"generate.mbean.comment.varFix", "Dans la MIB, cette variable est d\351finie comme une \"String\" de longueur fix\351e \340 {0}."},
        {"generate.mbean.comment.varOid", "La variable est identifi\351e par: \"{0}\"."},
        {"generate.mbean.comment.oid", "Le groupe est d\351fini avec un OID de: {0}."},
        {"generate.meta.comment.constr", "Constructeur pour les metadata associ\351s \340 \"{0}\"."},
        {"generate.meta.comment.create1", "La m\351thode permet la cr\351ation d'une entr\351e dans la table \340 travers l'appel au SET."},
        {"generate.meta.comment.create2", "Pour supporter un tel comportement, vous devez changer l'h\351ritage de cette classe."},
        {"generate.meta.comment.create3", "Elle doit d\351river de \"SnmpMibTableRemCreate\" fournie par le package SNMP."},
        {"generate.meta.comment.create4", "Dans cette classe, remplacez le \"extends SnmpMibTable\" par "},
        {"generate.meta.comment.create5", "\"extends SnmpMibTableRemCreate\" pour changer le comportement par d\351faut."},
        {"generate.meta.comment.create6", "Par d\351faut, le toolkit ne permet pas la cr\351ation d'une entr\351e dans une table "},
        {"generate.meta.comment.create7", "au travers d'une op\351ration de management. Ainsi la methode suivante n'est pas n\351cessaire."},
        {"generate.meta.comment.desc", "La classe est utilis\351e pour r\351presenter les metadata associ\351s au groupe \"{0}\"."},
        {"generate.meta.comment.checker", "Impl\351mente la m\351thode \"check\" deriv\351e de la classe abstraite SnmpMibNode."},
        {"generate.meta.comment.getter", "Impl\351mente la m\351thode \"get\" deriv\351e de la classe abstraite SnmpMibNode."},
        {"generate.meta.comment.getNext", "Impl\351mente la m\351thode \"get next\" deriv\351e de la classe abstraite SnmpMibNode."},
        {"generate.meta.comment.setter", "Impl\351mente la m\351thode \"set\" deriv\351e de la classe abstraite SnmpMibNode."},
        {"generate.meta.comment.setMoi", "Permet d'associer les metadata \340 un objet sp\351cifique."},
        {"generate.meta.comment.deprecated", "@deprecated Cette m\351thode n'est plus appel\351e."},
        {"generate.meta.comment.getvar", "Renvoie la valeur d'une variable scalaire"},
        {"generate.meta.comment.setvar", "Mets a jour la valeur d'une variable scalaire"},
        {"generate.meta.comment.checkvar", "V\351rifie la valeur d'une variable scalaire"},
        //        {"generate.meta.comment.getrequest", "Impl\351mente la m\351thode \"getEntryValues\" d\351riv\351e de la classe abstraite SnmpMibTable."},
        //        {"generate.meta.comment.setrequest", "Impl\351mente la m\351thode \"setEntryValues\" d\351riv\351e de la classe abstraite SnmpMibTable."},
        //        {"generate.meta.comment.checkrequest", "Impl\351mente la m\351thode \"checkEntryValues\" d\351riv\351e de la classe abstraite SnmpMibTable."},
        {"generate.meta.comment.isvariable", "Renvoie true si \"{0}\" identifie un objet scalaire."},
        {"generate.meta.comment.isreadable", "Renvoie true si \"{0}\" identifie un objet scalaire accessible en lecture."},
        {"generate.meta.comment.istable", "Renvoie true si \"{0}\" identifie une table."},
        {"generate.meta.comment.gettable", "Renvoie la table identifi\351e par \"{0}\"."},
        {"generate.meta.comment.getnextvarid", "Renvoie l''arc de l''objet colonne qui suit \"{0}\"."},
        {"generate.meta.comment.validatevarid", "V\351rifie que \"{0}\" identifie un objet colonne."},
        {"generate.meta.comment.table.index", "Construit l''index pour \"{0}\""},
        {"generate.meta.comment.table.constr", "Constructeur pour la table. Initialise  les metadata pour \"{0}\"."},
        {"generate.meta.comment.table.noRegistration", "La r\351f\351rence sur le MBean server n'est pas mise \340 jour et les entr\351es cr\351\351es via un SNMP SET ne seront pas enregistr\351es dans Java DMK."},
        {"generate.meta.comment.table.registration", "La r\351f\351rence sur le MBean server est mise \340 jour et les entr\351es cr\351\351es via un SNMP SET seront AUTOMATIQUEMENT enregistr\351es dans Java DMK."},
        {"generate.meta.comment.table.var", "R\351f\351rence sur les metadata de l'entr\351e."},
        {"generate.meta.comment.table.server", "R\351f\351rence sur le MBean server."},
        {"generate.mib.comment.header", "La classe est utilis\351e pour repr\351senter \"{0}\"."},
        {"generate.mib.comment.const", "Constructeur par d\351fault. Initialise l'arbre repr\351sentant la MIB."},
        {"generate.mib.comment.cmf", "Initialise les informations concernant le MBean server."},
        {"generate.mib.comment.modif", "Vous pouvez \351diter et modifier le fichier si vous voulez changer le comportement de la MIB."},
        {"generate.mib.comment.import", "D\351pendances sur le produit \"Java Dynamic Management Kit\"."},
        {"generate.mib.comment.init", "Initialisation de la MIB sans enregistrement dans le MBean server Java DMK."},
        {"generate.mib.comment.preRegister", "Initialisation de la MIB  avec enregistrement AUTOMATIQUE dans le MBean server Java DMK."},
        {"generate.mib.comment.oneCall", "N'authorise qu'une seule initialisation de la MIB."},
        {"generate.mib.comment.init.group", "Initialisation du groupe \"{0}\"."},
        {"generate.table.comment.getentries","Retourne les entr\351es stock\351es dans la table."},
        {"generate.table.comment.remove","Supprime l'entr\351e sp\351cifi\351e de la table."},
        {"generate.table.comment.add1","Ajoute une nouvelle entr\351e dans la table."},
        {"generate.table.comment.add2","Si le metadata associ\351 a besoin d'un ObjectName"},
        {"generate.table.comment.add3","un nouvel ObjectName sera g\351n\351r\351 grace \340 \"{0}\"."},
        {"generate.table.comment.calls","Cette m\351thode appelle la m\351thode \"{0}\" de \"{1}\"."},
        {"generate.mib.comment.init.support", "Pour supprimer le support de ce groupe, commentez la section suivante."},
        {"generate.meta.comment.setobjname", "D\351clare l'ObjectName du MBean correspondant \340 ce groupe ou cette entr\351e de table"},
        {"generate.meta.comment.checkset", "V\351rifie les droits d'acc\351s pour une op\351ration de SET"},
        {"generate.meta.comment.checkget", "V\351rifie les droits d'acc\351s pour une op\351ration de GET"},
        {"generate.meta.comment.buildatt", "Construit une valeur d'attribut comme celle retourn\351e par getValue(), \340 partir d'une SnmpValue"},
        {"generate.meta.comment.buildval", "Construit une SnmpValue \340 partir d'une valeur d'un objet Attribute retourn\351e par getValue()"},
        {"generate.meta.comment.getattname", "Retourne le nom de l''attribut correspondant \340 la variable SNMP identifi\351e par \"{0}\"."},
        {"generate.meta.comment.regtables", "Enregistre les objets SnmpMibTable appartenant au groupe dans le Meta objet qui lui correspond."},
        {"generate.meta.comment.table.objserver", "R\351f\351rence pointant sur le serveur d'objet SNMP."},
        {"generate.meta.comment.table.setobjsrv", "Permet de lier un Meta objet avec un serveur d'objet SNMP."},
        {"generate.meta.comment.impl.proposed1", "Une implementation par d\351faut pourrait \352tre par exemple:"},
        {"generate.meta.comment.impl.proposed2", "L'OID ne sera pas utile."},
        {"generate.mib.comment.init.support1", "Pour supprimer le support de ce groupe red\351finissez la "},
        {"generate.mib.comment.init.support2", "m\351thode de construction \"{0}()\", afin qu''elle renvoie \"null\""},
        {"generate.mib.comment.factory.abstract.meta", "M\351thode de construction pour la meta-classe du groupe \"{0}\"."},
        {"generate.mib.comment.factory.abstract.bean", "M\351thode de construction pour le MBean representant le groupe \"{0}\"."},
        {"generate.mib.comment.factory.text1.meta", "Vous pouvez red\351finir cette m\351thode si vous avez besoin de remplacer"},
        {"generate.mib.comment.factory.text1.bean", "Vous pouvez red\351finir cette m\351thode si vous avez besoin de remplacer"},
        {"generate.mib.comment.factory.text2.meta", "la meta-classe g\351n\351r\351e par d\351faut par votre propre classe modifi\351e."},
        {"generate.mib.comment.factory.text2.bean", "la classe du MBean g\351n\351r\351e par d\351faut par votre propre classe modifi\351e."},
        {"generate.mib.comment.factory.param.name", "@param groupName Nom du  groupe (\"{0}\")"},
        {"generate.mib.comment.factory.param.oid", "@param groupOid OID du groupe"},
        {"generate.mib.comment.factory.param.objname", "@param groupObjname ObjectName de ce groupe (peut \352tre null)"},
        {"generate.mib.comment.factory.param.server", "@param server MBeanServer pour ce groupe (peut \352tre null)"},
        {"generate.mib.comment.factory.return1.meta", "@return Une instance de la meta-classe g\351n\351r\351e pour"},
        {"generate.mib.comment.factory.return1.bean", "@return Une instance de la classe de MBean g\351n\351r\351e pour repr\351senter"},
        {"generate.mib.comment.factory.return2.meta", "        le groupe \"{0}\" ({1})"},
        {"generate.mib.comment.factory.return2.bean", "        le groupe \"{0}\" ({1})"},

        {"generate.mib.comment.factory.entry.abstract.meta", "M\351thode de construction pour la meta-classe des entr\351es de table \"{0}\"."},
        {"generate.mib.comment.factory.entry.abstract.bean", "M\351thode de construction pour les MBean repr\351sentant des entr\351es de table \"{0}\"."},
        {"generate.mib.comment.factory.entry.param.name", "@param snmpEntryName Nom de l''objet Entr\351e SNMP (conceptual row) (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.tablename", "@param tableName Nom de la table dans laquelle les entr\351es sont enregistr\351es (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.mib",  "@param mib L'objet SnmpMib dans lequel la table est enregistr\351e."},
        {"generate.mib.comment.factory.entry.param.server", "@param server Le MBeanServer pour les entr\351es de cette table (null est autoris\351)"},
        {"generate.mib.comment.factory.entry.return2.meta", "        les tuples \"{0}\" (conceptual row) ({1})"},
        {"generate.mib.comment.factory.entry.return2.bean", "        les tuples \"{0}\" (conceptual row)."},


        {"generate.mib.comment.factory.table.abstract.meta", "M\351thode de construction pour la meta-classe repr\351sentant la table \"{0}\"."},
        {"generate.mib.comment.factory.table.param.tablename", "@param tableName Nom de la table SNMP (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.groupname", "@param groupName Nom du groupe auquel appartient cette table (\"{0}\")"},
        {"generate.mib.comment.factory.entry.param.mib",  "@param mib L'objet SnmpMib dans lequel la table est enregistr\351e."},
        {"generate.mib.comment.factory.entry.param.server", "@param server Le MBeanServer pour les entr\351es de cette table (null est autoris\351)"},
        {"generate.mib.comment.factory.entry.return2.meta", "        la table \"{0}\" (conceptual row) ({1})"},

        {"generate.mib.comment.factory.note.return3.std", "Note: avec la version standard du metadata,"},
        {"generate.mib.comment.factory.note.return4.std", "l''interface \"{0}\" doit \352tre impl\351ment\351e"},
        {"generate.mib.comment.factory.note.return5.std", "par l'objet retourn\351."},
        {"generate.mib.comment.factory.note.return3.gen", "Note: avec la version g\351n\351rique du metadata,"},
        {"generate.mib.comment.factory.note.return4.gen", "soit l''interface \"{0}\", soit l''interface \"DynamicMBean\""},
        {"generate.mib.comment.factory.note.return5.gen", "peuvent \352tre impl\351ment\351ees par l'objet retourn\351."},

        {"generate.mib.comment.implements", "Impl\351mente la methode \"{0}\" d\351finie dans \"{1}\"."},
        {"generate.mib.comment.seedoc", "Voir l''API Javadoc de \"{0}\" pour plus de d\351tails."},

        {"generate.miboidtable.comment.header", "La classe contient les d\351finitions metadata pour \"{0}\"."},
        {"generate.miboidtable.comment.header2", "Appelez SnmpOid.setSnmpOidTable(new {0}()) pour charger les metadata dans la SnmpOidTable."},
    };

}
