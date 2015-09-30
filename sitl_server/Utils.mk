
# Verbosity Control ############################################################

V ?= 0

ifeq ($(V),1)
	quiet :=
	Q     :=
else
	quiet := quiet_
	Q     := @
	CABALFLAGS += -v0
	MAKEFLAGS  += -s --no-print-directory
endif

echo_cmd = $(if $($(quiet)cmd_$1),@echo "  $($(quiet)cmd_$1)";,)
cmd      = $(call echo_cmd,$1) $(cmd_$1)


# Commands #####################################################################

quiet_cmd_mkdir = MKDIR   $@
      cmd_mkdir = mkdir $@

quiet_cmd_rmdir = RMDIR   $(RM_TARGET)
      cmd_rmdir = $(RM) -r $(RM_TARGET)

quiet_cmd_cp    = CP      $< $(@)
      cmd_cp    = cp $< $@

quiet_cmd_rm    = RM      $(RM_TARGET)
      cmd_rm    = $(RM) $(RM_TARGET)

quiet_cmd_cabal-sandbox-init = SANDBOX $@
      cmd_cabal-sandbox-init = cabal $(CABALFLAGS) sandbox --sandbox=$@ init

quiet_cmd_cabal-install = INSTALL $(PACKAGE)
      cmd_cabal-install = cabal $(CABALFLAGS) install $(PACKAGE)
