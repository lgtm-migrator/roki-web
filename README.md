# roki-web

![CI](https://github.com/falgon/roki-web/workflows/CI/badge.svg)
[![Known Vulnerabilities](https://snyk.io/test/github/falgon/roki-web/badge.svg?targetFile=package.json)](https://snyk.io/test/github/falgon/roki-web?targetFile=package.json)
[![CodeFactor](https://www.codefactor.io/repository/github/falgon/roki-web/badge?s=e4b1f45b3bb2dc89c42f654d991238ef7771bc9f)](https://www.codefactor.io/repository/github/falgon/roki-web)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/0f3e0d5c5bbe43a78eaeed7485b72c7f)](https://www.codacy.com?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=falgon/roki-web&amp;utm_campaign=Badge_Grade)
[![Maintainability](https://api.codeclimate.com/v1/badges/e6574c3042df63bf6d41/maintainability)](https://codeclimate.com/github/falgon/roki-web/maintainability)
[![Language grade: JavaScript](https://img.shields.io/lgtm/grade/javascript/g/falgon/roki-web.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/falgon/roki-web/context:javascript)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Froki-web.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?ref=badge_shield)

The roki's website and blog.

* Website: [roki.dev](https://roki.dev)
    * Tech blog: [roki.dev/roki.log](https://roki.dev/roki.log/)
    * Diary: [roki.dev/roki.diary](https://roki.dev/roki.diary/)

## Usage

```sh
$ stack exec site -- --help
Usage: site [--version] [--preview] [-v|--verbose] [--internal-links] COMMAND
  The static site roki.dev compiler version 0.1.0.0 powerted by Hakyll

Available options:
  -h,--help                Show this help text
  --version                Show version
  --preview                Enable a preview flag
  -v,--verbose             Run in verbose mode
  --internal-links         Check internal links only

Available commands:
  build                    Generate the site
  check                    Validate the site output
  clean                    Clean up and remove cache
  deploy                   Upload/deploy roki.dev
  rebuild                  Clean and build again
  server                   Start a preview server
  watch                    Autocompile on changes and start a preview server

$ stack exec site -- build --preview # fast build (This does not render KaTeX)
$ stack exec site -- build # release build
```

## System overview

### Overview of blog posts and website system updates

<p align="center">
<img src="https://user-images.githubusercontent.com/1241783/90969880-d99b8a00-e538-11ea-8f35-684365e14406.png" width="640" alt="system overview" />
</p>

* [roki-web](https://github.com/falgon/roki-web) (this repository)
* [roki-web-post](https://github.com/falgon/roki-web-post) (private repository)

[GitHub Actions for GitHub pages](https://github.com/peaceiris/actions-gh-pages)
has been very helpful in building this system. 
Thanks for it.

### Overview of preview function accompanying PR

<p align="center">
<img src="https://user-images.githubusercontent.com/1241783/92309894-3fc9e780-efe4-11ea-88f2-29697c54b156.png" height="450px" alt="pr" />
</p>

When PR is issued, artifact is built on CircleCI as shown above and it is possible to preview.
Also, the bot [@kiirotori](https://github.com/kiirotori) will add a comment containing the URL of the preview site and the URL of the circleci JOB log that can be displayed in the artifact.

## History

This is a project for a new website that integrates the following two sites.

* [Roki Profile](https://falgon.github.io/roki/)
    * Repo: <https://github.com/falgon/roki>
* [roki.log](https://falgon.github.io/roki.log/)
    * Repo: <https://bitbucket.org/r0ki/roki.log/> (private)
    * Repo (gh-page): <https://github.com/falgon/roki.log>

These two will be discontinued in the future and replaced by this new website and
have the following characteristics.

* The website [Roki Profile](https://falgon.github.io/roki/) is deployed manually (maintained by snyk-bot)
* The blog [roki.log](https://falgon.github.io/roki.log/) is deployed bitbucket-pipelines 

I used bitbucket, a kind of Git service that I can use private repositories for free, 
because github couldn't use private repositories for free before.
But times have changed and now private repositories are now available for free and 
native CI (GitHub Actions) are also available.
In this project, I use [Hakyll](https://jaspervdj.be/hakyll/), which is a static site generator, 
to manage the frontend library with node.js and have it managed with synk-bot. 
Deployment is done with github actions, drafts of blog posts are managed in another repository, 
and commits to a specific branch in that repository are triggered and pushed to this branch.
This makes it possible to make open source and keep drafts private, 
improving and integrating the previously separated management form.

## License [![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Froki-web.svg?type=small)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?ref=badge_small) 

Licenses of this project is managed by [FOSSA](https://fossa.com/) (you can see above "license scan" badge).
For more details, you can see [the dependency report of FOSSA](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?utm_source=share_link).

[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Froki-web.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Froki-web?ref=badge_large)