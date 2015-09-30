
all:

include Utils.mk

# Config
include Config.mk


# Derived Config ###############################################################

ARDUCOPTER_PATH := $(ARDUPILOT_PATH)/ArduCopter

SERVER_ARGS ?=


# Entry Target #################################################################

.PHONY: all

all: $(ARDUCOPTER_PATH)/ArduCopter.elf $(BUILD_DIR)/server | $(BUILD_DIR)/env


# Building #####################################################################

$(BUILD_DIR):
	$(call cmd,mkdir)

.PHONY: setup-virtualenv

# Setup the python environment
$(BUILD_DIR)/env: | $(BUILD_DIR)
	$(Q)virtualenv --system-site-packages $(BUILD_DIR)/env \
		&& BUILD_DIR=$(BUILD_DIR) scripts/setup.sh

$(ARDUPILOT_PATH)/config.mk:
	$(Q)$(MAKE) -C $(ARDUCOPTER_PATH) configure

$(ARDUCOPTER_PATH)/ArduCopter.elf: $(ARDUPILOT_PATH)/config.mk
	$(Q)$(MAKE) -C $(ARDUCOPTER_PATH) sitl

$(BUILD_DIR)/server: $(BUILD_DIR)/cabal-sandbox/bin/server
	$(call cmd,cp)


SERVER_DEPS := ./server/server.cabal $(shell find ./server/src -name \*.hs)

$(BUILD_DIR)/cabal-sandbox/bin/server: $(SERVER_DEPS)
$(BUILD_DIR)/cabal-sandbox/bin/server: PACKAGE := ./server
$(BUILD_DIR)/cabal-sandbox/bin/server: | $(BUILD_DIR)/cabal-sandbox
	$(call cmd,cabal-install)

$(BUILD_DIR)/cabal-sandbox: | $(BUILD_DIR)
	$(call cmd,cabal-sandbox-init)
	$(Q)cabal sandbox add-source server
	$(Q)cabal install alex happy


# Running ######################################################################

.PHONY: run

run: $(ARDUCOPTER_PATH)/ArduCopter.elf $(BUILD_DIR)/server | $(BUILD_DIR)/env
	$(BUILD_DIR)/server \
	    --build-path=$(abspath $(BUILD_DIR)) \
	    --ardupilot-path=$(abspath deps/ardupilot) \
	    $(SERVER_ARGS)


# SITL Testing #################################################################

.PHONY: test

test: $(ARDUCOPTER_PATH)/ArduCopter.elf $(BUILD_DIR)/server | $(BUILD_DIR)/env
	$(BUILD_DIR)/server \
	    --build-path=$(abspath $(BUILD_DIR)) \
	    --ardupilot-path=$(abspath deps/ardupilot) \
	    --test

sitl-%: $(ARDUCOPTER_PATH)/ArduCopter.elf $(BUILD_DIR)/server | $(BUILD_DIR)/env
	tmux new-window -c '$(PWD)' -n sitl-$* \
	    'scripts/sitl.sh "$(abspath $(BUILD_DIR))" "$(abspath $(ARDUPILOT_PATH))" $*'


# Cleaning #####################################################################

.PHONY: clean clean-build
clean: clean-arducopter

rm-build: RM_TARGET := $(BUILD_DIR)
rm-build:
	$(call cmd,rmdir)


.PHONY: distclean
distclean: clean rm-build

.PHONY: clean-arducopter
clean-arducopter: rm-ArduCopter.elf
	$(Q)$(MAKE) -C $(ARDUCOPTER_PATH) clean

.PHONY: rm-ArduCopter.elf
rm-ArduCopter.elf: RM_TARGET=$(ARDUCOPTER_PATH)/ArduCopter.elf
rm-ArduCopter.elf:
	$(call cmd,rm)


# Debugging ####################################################################

print-%:
	@echo '$* = $($*)'
