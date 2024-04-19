.PHONY: run ghcid

run:
	@./az.hs

ghcid:
	@ghcid -c 'stack ghci az.hs'
