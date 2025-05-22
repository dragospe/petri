HOOGLE_PROJECT_PORT := 8080
HOOGLE_DEPS_PORT := 8081

################################################################################
# Code
FIND_EXCLUDE_PATH := -not -path './dist-*/*'

FIND_HASKELL_SOURCES := find -name '*.hs' $(FIND_EXCLUDE_PATH)
FIND_NIX_SOURCES := find -name '*.nix' $(FIND_EXCLUDE_PATH)
FIND_CABAL_SOURCES := find -name '*.cabal' $(FIND_EXCLUDE_PATH)

# Runs as command on all results of the `find` call at one.
# e.g.
#   foo found_file_1 found_file_2
find_exec_all_fn = $(1) -exec $(2) {} +

# Runs a command on all results of the `find` call one-by-one
# e.g.
#   foo found_file_1
#   foo found_file_2
find_exec_one_by_one_fn = $(1) | xargs -i $(2) {}

.PHONY: format
format: format_haskell format_nix format_cabal
format_check : format_check_haskell format_check_nix format_check_cabal

# Run stylish-haskell of .hs files
.PHONY: format_haskell
format_haskell: 
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), fourmolu -c -m inplace)

# Apply hlint suggestions
.PHONY: lint
lint: 
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), hlint -j --refactor --refactor-options="-i")

# Check hlint suggestions
.PHONY: lint_check
lint_check: 
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), hlint -j)
