init:
	@npm i
	@stack build

watch:
	@stack build
	@stack exec site -- build --preview
	@stack exec site -- check --internal-links
	@stack exec site -- watch --preview

.PHONY: init watch
