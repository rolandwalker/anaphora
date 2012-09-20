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
TEST_DIR=ert-tests
TEST_LIB=ert.el
TEST_LIB_URL=http://bzr.savannah.gnu.org/lh/emacs/emacs-24/download/head:/ert.el-20110112160650-056hnl9qhpjvjicy-2/ert.el
CURL_COMMAND=curl

build :
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile *.el

working-test-lib :
	@cd $(TEST_DIR)                            && \
	$(EMACS) $(EMACS_FLAGS)  -L . -L .. -l ert || \
	(echo "$(TEST_LIB) not loaded, run 'make downloads' to fetch it" ; exit 1)

downloads :
	$(CURL_COMMAND) '$(TEST_LIB_URL)' > $(TEST_DIR)/$(TEST_LIB)

test : working-test-lib
	@cd $(TEST_DIR)                                 && \
	(for testlib in *-test.el; do                      \
	    $(EMACS) $(EMACS_FLAGS) -L . -L .. -l cl -l ert -l $$testlib --eval \
	    '(flet ((ert--print-backtrace (&rest args)     \
	      (insert "no backtrace in batch mode")))      \
	       (ert-run-tests-batch-and-exit))' || exit 1; \
	done)

autoloads :
	$(EMACS) $(EMACS_FLAGS) --eval '(let ((generated-autoload-file "$(WORK_DIR)/loaddefs.el")) (update-directory-autoloads "$(WORK_DIR)"))'

clean :
	@rm -f loaddefs.el *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_LIB)
