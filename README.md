# Roki Website and Blog

## History

This is a project for a new website that integrates the following two sites.

* [Roki Profile](https://falgon.github.io/roki/)
    * Project: <https://github.com/falgon/roki>
* [roki.log](https://falgon.github.io/roki.log/)
    * Project: <https://bitbucket.org/r0ki/roki.log/> (secret)

These two will be discontinued in the future and replaced by this new website and
have the following characteristics.

* The website [Roki Profile](https://falgon.github.io/roki/) is deployed manually (maintained by snyk-bot)
* The blog [roki.log](https://falgon.github.io/roki.log/) is deployed bitbucket-pipelines 

I used bitbucket, a kind of Git service that I can use private repositories for free, 
because github couldn't use private repositories for free before.
But times have changed and now private repositories are now available for free and 
native CI (GitHub Actions) are also available.
In this project, I use hakyll, which is a static site generator, 
to manage the frontend library with node.js and have it managed with synk-bot. 
Deployment is done with github actions, drafts of blog posts are managed in another repository, 
and commits to a specific branch in that repository are triggered and pushed to this branch.
This makes it possible to make open source and keep drafts private, 
improving and integrating the previously separated management form.
