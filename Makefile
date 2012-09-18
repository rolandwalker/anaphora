EMACS=emacs
# EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
# EMACS=/Applications/Emacs23.app/Contents/MacOS/Emacs
# EMACS=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
# EMACS=/Applications/Macmacs.app/Contents/MacOS/Emacs
# EMACS=/usr/local/bin/emacs
# EMACS=/opt/local/bin/emacs
# EMACS=/usr/bin/emacs
EMACS_FLAGS=-Q --batch

WORK_DIR=$(shell pwd)
TEST_OUTPUT=test.out
TEST_DIR=expectations

build :
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile *.el

test :
	@cd $(TEST_DIR)          && \
	rm -f $(TEST_OUTPUT)     && \
	$(EMACS) $(EMACS_FLAGS) -L . -L .. -l el-expectations -f batch-expectations $(TEST_OUTPUT) *.el
	@test -e $(TEST_DIR)/$(TEST_OUTPUT) && cat $(TEST_DIR)/$(TEST_OUTPUT)

autoloads :
	$(EMACS) $(EMACS_FLAGS) --eval '(let ((generated-autoload-file "$(WORK_DIR)/loaddefs.el")) (update-directory-autoloads "$(WORK_DIR)"))'

clean :
	@rm -f loaddefs.el *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_OUTPUT)
