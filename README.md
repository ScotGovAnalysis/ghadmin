# ghadmin

Code to assist with administration of GitHub organisation

## Authentication

This package uses the [gh package](https://gh.r-lib.org/) to use the [GitHub API](https://docs.github.com/en/rest). This requires the use of a GitHub Personal Access Token (PAT). 

To check whether you have a PAT stored, run `gh::gh_token()`. If a PAT can't be found, this will return an empty string (`""`). To create a new token, run `usethis::create_github_token()`.

More information is available in the [gh documentation](https://gh.r-lib.org/articles/managing-personal-access-tokens.html).

## Process

An [outline of the process](reviews/review-process.md) followed for previous membership reviews can be found in the `reviews` folder.

## Licence

Unless stated otherwise, the codebase is released under [the MIT License](LICENCE). This covers both the codebase and any sample code in the documentation.

The documentation is [© Crown copyright](http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms of the [Open Government 3.0](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) licence.
