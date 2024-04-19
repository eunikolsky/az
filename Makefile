.PHONY: run ghcid

run:
	@./az.hs

ghcid:
	@# `ghcid -c 'stack ghci az.hs'` also works until you start using a new package that wasn't built yet; then `m run` fixes that by building it; providing the packages from the file also asks `stack` to build them
	@ghcid -c "stack ghci $$( rg -F -m1 'stack script' az.hs | sed -nE 's/.*stack script (.*)/\1/p' ) az.hs"
