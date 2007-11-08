#
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
# 
# Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
# 
# The contents of this file are subject to the terms of either the GNU
# General Public License Version 2 only ("GPL") or the Common Development
# and Distribution License("CDDL") (collectively, the "License").  You
# may not use this file except in compliance with the License. You can obtain
# a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
# or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
# language governing permissions and limitations under the License.
# 
# When distributing the software, include this License Header Notice in each
# file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
# Sun designates this particular file as subject to the "Classpath" exception
# as provided by Sun in the GPL Version 2 section of the License file that
# accompanied this code.  If applicable, add the following below the License
# Header, with the fields enclosed by brackets [] replaced by your own
# identifying information: "Portions Copyrighted [year]
# [name of copyright owner]"
# 
# Contributor(s):
# 
# If you wish your version of this file to be governed by only the CDDL or
# only the GPL Version 2, indicate your decision by adding "[Contributor]
# elects to include this software in this distribution under the [CDDL or GPL
# Version 2] license."  If you don't indicate a single choice of license, a
# recipient has the option to distribute your version of this file under
# either the CDDL, the GPL Version 2 or to extend the choice of license to
# its licensees as provided above.  However, if you add GPL Version 2 code
# and therefore, elected the GPL Version 2 license, then the option applies
# only if the new code is made subject to such option by the copyright
# holder.
#

# Linux 2.1 AR and DLL targets

#
# AR[n]_TARGET, AR[n]_OBJS 
#

ifdef AR_TARGET
AR_OBJ_INT=$(addsuffix .$(OBJ),$(AR_OBJS))
REAL_AR_OBJS=$(addprefix $(OBJDIR)/,$(AR_OBJ_INT))
$(OBJDIR)/$(LIBPREFIX)$(AR_TARGET).$(STATIC_LIB_SUFFIX): $(REAL_AR_OBJS)
	$(RM) -f $@
	$(AR) -r $@ $(REAL_AR_OBJS) $(AR_NONPARSED_OBJS)
endif

ifdef AR1_TARGET
AR1_OBJ_INT=$(addsuffix .$(OBJ),$(AR1_OBJS))
REAL_AR1_OBJS=$(addprefix $(OBJDIR)/,$(AR1_OBJ_INT))
$(OBJDIR)/$(LIBPREFIX)$(AR1_TARGET).$(STATIC_LIB_SUFFIX): $(REAL_AR1_OBJS)
	$(RM) -f $@
	$(AR) -r $@ $(REAL_AR1_OBJS) $(AR1_NONPARSED_OBJS)
endif

ifdef AR2_TARGET
AR2_OBJ_INT=$(addsuffix .$(OBJ),$(AR2_OBJS))
REAL_AR2_OBJS=$(addprefix $(OBJDIR)/,$(AR2_OBJ_INT))
$(OBJDIR)/$(LIBPREFIX)$(AR2_TARGET).$(STATIC_LIB_SUFFIX): $(REAL_AR2_OBJS)
	$(RM) -f $@
	$(AR) -r $@ $(REAL_AR2_OBJS) $(AR2_NONPARSED_OBJS)
endif

ifdef AR3_TARGET
AR3_OBJ_INT=$(addsuffix .$(OBJ),$(AR3_OBJS))
REAL_AR3_OBJS=$(addprefix $(OBJDIR)/,$(AR3_OBJ_INT))
$(OBJDIR)/$(LIBPREFIX)$(AR3_TARGET).$(STATIC_LIB_SUFFIX): $(REAL_AR3_OBJS)
	$(RM) -f $@
	$(AR) -r $@ $(REAL_AR3_OBJS) $(AR3_NONPARSED_OBJS)
endif

ifdef BSC_TARGET
$(BSC_TARGET): ; \
	$(ECHO) The $@ file is for NT only. > $@
endif

ifdef SB_INIT
$(SB_INIT): ; \
	$(ECHO) export / into $(SB_DIR) > $@
endif

EXPORT__LIBS=$(EXPORT_LIBRARIES) $(EXPORT_DYNAMIC_LIBRARIES)

ifdef notdef
#
# DLL[n]_TARGET, DLL[n]_OBJS, [ DLL[n]_EXTRA ], [ DLL[n]_LIBS ]
#

ifdef DLL_TARGET
DLL_REAL_OBJS:=$(addprefix $(OBJDIR)/, $(DLL_OBJS:=.$(OBJ)))
DLL_OUTPUT_FILE:=$(OBJDIR)/$(LIBPREFIX)$(DLL_TARGET).$(DYNAMIC_LIB_SUFFIX)
$(DLL_OUTPUT_FILE): $(DLL_REAL_OBJS) 
	$(LD) $(CC_SHARED_LIB_FLAGS) $(LD_DYNAMIC) \
		\
		$(LD_DASH_O)$(DLL_OUTPUT_FILE) \
		\
		$(DLL_REAL_OBJS) $(DLL_NONPARSED_OBJS) \
                $(DLL_EXTRA) $(PRELIB) $(LD_FLAGS) \
		$(DLL_REAL_LIBS) $(DLL_NONPARSED_LIBS) $(LD_LIBS) $(LD_RPATHS)
endif #DLL_TARGET

ifdef DLL1_TARGET
DLL1_REAL_OBJS = $(addprefix $(OBJDIR)/,$(DLL1_OBJS:=.$(OBJ)))
DLL1_OUTPUT_FILE= $(OBJDIR)/$(LIBPREFIX)$(DLL1_TARGET).$(DYNAMIC_LIB_SUFFIX)
$(DLL1_OUTPUT_FILE): $(DLL1_REAL_OBJS)
	$(LD) $(LD_DYNAMIC) \
		\
		$(LD_DASH_O)$(DLL1_OUTPUT_FILE) \
		\
		$(DLL1_REAL_OBJS) $(DLL1_NONPARSED_OBJS) \
                $(DLL1_EXTRA) $(PRELIB) $(LD_FLAGS) \
		$(DLL1_REAL_LIBS) $(DLL1_NONPARSED_LIBS) $(LD_LIBS) $(LD_RPATHS)
endif #DLL1_TARGET

ifdef DLL2_TARGET
DLL2_REAL_OBJS = $(addprefix $(OBJDIR)/,$(DLL2_OBJS:=.$(OBJ)))
DLL2_OUTPUT_FILE= $(OBJDIR)/$(LIBPREFIX)$(DLL2_TARGET).$(DYNAMIC_LIB_SUFFIX)
$(DLL2_OUTPUT_FILE): $(DLL2_REAL_OBJS)
	$(LD) $(LD_DYNAMIC) \
		\
		$(LD_DASH_O)$(DLL2_OUTPUT_FILE) \
		\
		$(DLL2_REAL_OBJS) $(DLL2_NONPARSED_OBJS) \
                $(DLL2_EXTRA) $(PRELIB) $(LD_FLAGS) \
		$(DLL2_REAL_LIBS) $(DLL2_NONPARSED_LIBS) $(LD_LIBS) $(LD_RPATHS)
endif #DLL2_TARGET

endif # notdef
