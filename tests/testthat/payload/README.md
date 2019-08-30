## Example payloads

At the moment we have 3 different sets of example payloads all with slightly different paths
* payloads here - path relative to hintr/tests/testthat (the location that testthat tests are run from)
* hintr/exmaples/docker_* - paylods for testing the API running from within docker
* hintr/examples/* - payloads for testing the API when running by calling `api()` from a local R session

TODO: Consolidate these all into one by adding the ability to specify the root directory for each of the requests.
We can then hopefully run up the api with a different root for each of the 3 sources so these examples don't have to exist in many different places.
