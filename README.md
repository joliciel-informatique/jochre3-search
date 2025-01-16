# Jochre Search Server

The Jochre Search server provides OCR search functionality.

This search engine is built on top of [Apache Lucene](https://lucene.apache.org/), and stores coordinates for all words in a separate database.
When a search is performed, it enables query extension to all words sharing the same lemma as the search term.
Using the stored word coordinates, it can also highlight terms matching the search results in the original image.
Finally, it includes functionality to allow users to crowd-source OCR corrections, as well as corrections to the book's metadata.

Note that the Jochre 3 OCR search engine is completely independent from the Jochre 3 OCR generation software.
All it requires is a PDF (or a set of page images) and corresponding Alto files produced by any OCR tool.

If you use this software in your studies, please cite the [following article](https://arxiv.org/abs/2501.08442):

```bibtex
@misc{urieli2025jochre3yiddishocr,
      title={Jochre 3 and the Yiddish OCR corpus},
      author={Assaf Urieli and Amber Clooney and Michelle Sigiel and Grisha Leyfer},
      year={2025},
      eprint={2501.08442},
      archivePrefix={arXiv},
      primaryClass={cs.CL},
      url={https://arxiv.org/abs/2501.08442},
}
```

## Setup

First, open the console and clone the project using `git` (or you can simply download the project) and then change the directory:

```shell
git clone git@gitlab.com:joliciel/jochre3-search.git
cd jochre3-search
```

## Running the server as a docker image

Navigate to the project directory, and create a file `docker-compose/jochre.conf` based on `docker-compose.jochre-sample.conf`.

Run the docker-compose script:

```shell
docker-compose -p jochre -f docker-compose/jochre3-search.yml up
```

You can then navigate to the Swagger documentation as follows: http://localhost:4242/docs/

If the application is setup for raw authorization, click on `Authorize`, and enter the following string:

```json
{ "username": "Test", "email": "test@example.com", "roles": ["index"] }
```

## Running the server locally

Navigate to the project directory, and run the application as follows:

```shell
make init-dev-env
sbt
project api
run
```

You can then navigate to the Swagger documentation as follows: http://localhost:4242/docs/

See above for authorization.

## Building the docker image

Create your environment variables:

```shell
export JOCHRE3_DOCKER_REGISTRY=registry.gitlab.com
export JOCHRE3_DOCKER_USERNAME=assafurieli@gmail.com
```

Either create an additional JOCHRE3_DOCKER_PASSWORD variable, or (more secure) add the password to pass:

```shell
pass insert jochre/sonatype_deploy
```

Run the publish script

```shell
make publish-image
```

## Continuous integration on GitLab

For the CI script in [.gitlab-ci.yml](.gitlab-ci.yml) to work, you first need to set up a runner with `gitlab-runner` (the docker image below needs to correspond to the one in the yml file):

```shell
sudo gitlab-runner register \
  --non-interactive \
  --url "https://gitlab.com/" \
  --registration-token $REGISTRATION_TOKEN \
  --executor "docker" \
  --description "Docker runner" \
  --docker-image "docker:24" \
  --docker-privileged
```

It will automatically deploy a docker image on tags, on the condition that you add two variables to the Gitlab CI settings: `JOCHRE3_DOCKER_USERNAME` and `JOCHRE3_DOCKER_PASSWORD`.
