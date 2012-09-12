EMACS=emacs
# EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
# EMACS=/Applications/Emacs23.app/Contents/MacOS/Emacs
# EMACS=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
# EMACS=/Applications/Macmacs.app/Contents/MacOS/Emacs
# EMACS=/usr/local/bin/emacs
# EMACS=/opt/local/bin/emacs
# EMACS=/usr/bin/emacs
EMACS_FLAGS=-Q --batch

TEST_OUTPUT=test.out
TEST_DIR=expectations

build :
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile *.el

test :
	@cd $(TEST_DIR)          && \
	rm -f $(TEST_OUTPUT)     && \
	$(EMACS) $(EMACS_FLAGS) -L . -L .. -l el-expectations -f batch-expectations $(TEST_OUTPUT) *.el
	@test -e $(TEST_DIR)/$(TEST_OUTPUT) && cat $(TEST_DIR)/$(TEST_OUTPUT)

clean :
	@rm -f *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_OUTPUT)
