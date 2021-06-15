init:
	@nvm install
	@nvm use
	@npm i
	@stack build

watch:
	@stack build
	@stack exec site -- build --preview
	@stack exec site -- check --internal-links
	@stack exec site -- watch --preview

watch-prebuild:
	@cat ${HOME}/.ghcr.txt | docker login docker.pkg.github.com -u falgon --password-stdin
	@docker pull docker.pkg.github.com/falgon/roki-web/roki-web-env:latest
	@pushd ./docker \
		&& docker-compose -f docker-compose-ghpr.yml up -d preview \
		&& docker-compose -f docker-compose-ghpr.yml logs -f preview \
		; popd

stop-watch-prebuild:
	@pushd ./docker \
		&& docker-compose -f docker-compose-ghpr.yml stop preview

.PHONY: init watch watch-prebuild
