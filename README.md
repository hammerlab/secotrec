Secotrec: Deploy Coclobas/Ketrew with class.
============================================


`secotrec` is a library, it provides a bunch of hacks to create more or less
generic deployments.

It comes with 3 preconfigured “examples”. You should pick one.

1. `secotrec-gke`:
    - sets up a new GCloud host with a bunch of services:
      Postgresql, Ketrew, Coclobas (in GKE mode), Tlstunnel (with valid
      domain-name and certificate), and optionally an `nginx` basic-auth HTTP
      proxy.
    - creates an new NFS server.
    - it also deals with the GKE cluster, firewall rules, etc.
2. `secotrec-local`: sets up Postgresql, Ketrew, and Coclobas (in local docker
   mode) running locally or on a fresh GCloud box.
3. `seotrec-aws`: still experimental, sets up a similar deployment as
   `secotrec-gke` based on AWS-Btatch.

The examples have the option of preparing the default biokepi-work directory
with
[`b37decoy_20160927.tgz`](https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz) and
[`b37_20161007.tgz`](https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37_20161007.tgz).

For other administration purposes, we also provide `secotrec-make-dockerfiles`,
as `Dockerfile` generation tool.

This file provides detailed usage information, for high-level
*tutorial-oriented* documentation please check-out the
[`hammerlab/wobidisco`](https://github.com/hammerlab/wobidisco) project.  A good
starting point is the “Running Local”
[tutorial](https://github.com/hammerlab/wobidisco/blob/master/doc/running-local.md).


Install
-------

You can install secotrec either from Opam or from a Docker image.

### Option 1: With Opam

If you have an `opam` environment, for now we need a few packages pinned:

    opam pin -n add ketrew https://github.com/hammerlab/ketrew.git
    opam pin -n add biokepi https://github.com/hammerlab/biokepi.git
    opam pin -n add secotrec https://github.com/hammerlab/secotrec.git
    opam upgrade
    opam install tls secotrec biokepi

Notes:

- We need `tls` to submit jobs to the deployed Ketrew server (`prepare` and
  `test-biokepi-machine` sub-commands).
- `biokepi` is only used by generated code (biokepi machine and its test).


### Option 2: Dockerized

#### Getting the Docker image

    # Get the docker image
    docker pull hammerlab/keredofi:secotrec-default

#### Setup for secrotec-gke

    # Enter the container for GKE use case
    docker run -e KETREW_CONFIGURATION -it hammerlab/keredofi:secotrec-default

#### Setup secrotec-local

If you've chosen to use secotrec-local:

    # Enter the container for local use case
    docker run \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -e KETREW_CONFIGURATION \
      -it hammerlab/keredofi:secotrec-default \
      bash

If you do use `secotrec-local`, please mind that we cannot access the Ketrew
server from the current container (which is in a different network). We can jump
to another container which is in the right network:

    secotrec-local docker-compose exec coclo opam config exec bash

#### Configure your gcloud utility

Once you are inside the container, you first need to configure your gcloud
utilities for proper access to GCloud services:

    # Login to your acount
    gcloud auth login

    # Set your GKE project
    gcloud config set project YOUR_GKE_PROJECT

Usage
-----

### Secotrec-gke

#### Configure

Generate a template configuration file:

```
secotrec-gke generate-configuration my-config.env
```

Edit the nicely documented `my-config.env` file
until `. my-config.env ; secotrec-gke print-configuration` is happy and you are
too.

**Note:** if you decide to use HTTP-Basic-Auth (`htpasswd` option), you will
need to append a user-name and password to some of the commands below
(see the optional `--username myuser --password mypassword` arguments).

#### Deploy

Then just bring everything up (can take about 15 minutes):

```
. my-config.env
secotrec-gke up
```

Check:

```
secotrec-gke status
```

Output should say that all services are up:

```
...
       Name                      Command               State           Ports
-------------------------------------------------------------------------------------
coclotest_coclo_1     opam config exec -- /bin/b ...   Up      0.0.0.0:8082->8082/tcp
coclotest_kserver_1   opam config exec -- dash - ...   Up      0.0.0.0:8080->8080/tcp
coclotest_pg_1        /docker-entrypoint.sh postgres   Up      0.0.0.0:5432->5432/tcp
coclotest_tlstun_1    opam config exec -- sh -c  ...   Up      0.0.0.0:443->8443/tcp
...
```

It should show some more interesting information (incl. Ketrew WebUI URL(s)), if
there is no display like:

```
SECOTREC: Getting Ketrew Hello:
Curl ketrew/hello says: ''
[
  "V0",
  [
    "Server_status",
    {
    ...
    /* A few lines of JSON */
...
```

it means that Ketrew is not (yet) ready to answer (depending on Opam-pins and
configuration; the server may not be up right away).

#### Prepare/Test

The deployment is usable as is, but to use it with Biokepi more efficiently
one can start the “preparation” Ketrew workflow:

```
secotrec-gke prepare [--username myuser --password mypassword]
```

and go baby-sit the Ketrew workflow on the WebUI.

There is also a test of the Biokepi-Machine (for now, it can be run concurrently
to the preparation workflow):

```
secotrec-gke test-biokepi-machine [--username myuser --password mypassword]
```

the workflow uses Coclobas/Kubernetes.

#### Generate Ketrew/Biokepi Stuff

Generate a Ketrew client configuration:

```
secotrec-gke ketrew-configuration /tmp/kc.d [--username myuser --password mypassword]
```

(then you can use `export KETREW_CONFIG=/tmp/kc.d/configuration.ml`).

Generate a Biokepi `Machine.t`:

```
secotrec-gke biokepi-machine /tmp/bm.ml [--username myuser --password mypassword]
```

#### More Inspection / Tools

We can have a `top`-like display:

```
secotrec-gke top
```

We can talk directly to the database used by Ketrew and Coclobas:

```
secotrec-gke psql
```

The subcommand `docker-compose` (alias `dc`) forwards its arguments to
`docker-compose` on the node, with the configuration, e.g.:

```
secotrec-gke dc ps
secotrec-gke dc logs coclo
secotrec-gke dc exec kserver ps aux
secotrec-gke get-coclobas-logs somewhere.tar.gz
...
# See:
secotrec-gke --help
```

#### Destroy

Take down everything (including the Extra-NFS server and its storage)
with the following command:

```
secotrec-gke down --confirm

```

Note that this action requires the additional `--confirm` argument
to prevent destroying the secotrec setup accidentally. `-y`, `--yes`,
`--really`, and `--please` are other alternatives that can be
used to confirm the destroy procedure.


### Secotrec-local

Configuration is all optional (the `gcloud` version adds some constrains;
cf. the generated config-file), but it works the same way as Secotrec-GKE:

```
secotrec-local generate-configuration my-config.env
# Edit my-config.env
source my-config.env
secotrec-local print-configuration
secotrec-local up
secotrec-local status
```

Other commands work as well:

```
secotrec-local top       # top-like display of the containers
...
secotrec-local prepare    # submits the preparation workflow to Ketrew (get `b37decoy`)
...
secotrec-local ketrew-configuration /path/to/config/dir/     # generate a Ketrew config
secotrec-local biokepi-machine /path/to/biokepi-machine.ml   # generate a Biokepi config/machine
...
secotrec-local test biokepi-machine
...
secotrec-local down
```

### Secotrec-Make-Dockerfiles

`secotrec-make-dockerfiles` is designed to update the Docker-Hub images
at [`hammerlab/keredofi`](https://hub.docker.com/r/hammerlab/keredofi/builds/).

The `README.md` and the corresponding branches of GitHub
repository [`hammerlab/keredofi`](https://github.com/hammerlab/keredofi) are
also updated for convenience by this tools (but we do not use Docker-Hub
automated builds any more).

Display all the Dockerfiles on stdout:

    secotrec-make-dockerfiles view

Write the `Dockerfile`s in their respective branches and commit if something
changed:

    secotrec-make-dockerfiles write --path=/path/to/keredofi

when done, the tool displays the Git graph of the Keredofi repo; if you're
happy, just go there and `git push --all`.


Submit a Ketrew workflow that builds and runs some tests on all the
`Dockerfiles` (for now this expects a `secotrec-local`-like setup):

    eval `secotrec-local env`
    secotrec-make-dockerfiles test

See `secotrec-make-dockerfiles test --help` for more options, you can for
instance push to the Docker-Hub:

    secotrec-make-dockerfiles test \
        --repo hammerlab/keredofi-test-2 --push agent-cooper,black-lodge


### Secotrec-aws-node

When in the environment `WITH_AWS_NODE` is `true`, and application
`secotrec-aws-node` is built, see:

    secotrec-aws-node --help

For now the this uses the AWS API to setup a “ready-to-use” EC2 server.

The build requires `master` versions of: `aws` and `aws-ec2`:

```shell
pin_all () {
    local tmpdir=$HOME/tmp/
    rm -fr $tmpdir/ocaml-aws
    cd $tmpdir
    git clone https://github.com/inhabitedtype/ocaml-aws.git
    cd ocaml-aws
    opam pin add -y -n aws .
    for lib in $(find libraries/ -type d -name opam) ; do
        echo "Do $lib"
        opam pin add -y -n aws-$(basename $(dirname $lib)) ./$(dirname $lib)/
    done;
}
installs () {
    local all="aws aws-ec2"
    opam remove -y $all
    for lib in $all ; do
        ocamlfind remove $lib
    done
    opam install -y $all
}
```

(cf. also
[comment](https://github.com/inhabitedtype/ocaml-aws/issues/21#issuecomment-283446276)
on [`#21`](https://github.com/inhabitedtype/ocaml-aws/issues/21)).

Usage:

    # Set your AWS credentials:
    export AWS_KEY_ID=AKSJDIEJDEIJDEKJJKXJXIIEJDIE
    export AWS_SECRET_KEY=dliejsddlj09823049823sdljsd/sdelidjssleidje
    # Configure once:
    secotrec-aws-node config --path /path/to/store/config-and-more/ \
        --node-name awsuser-dev-0 \
        --ports 22,443 \
        --region us-east-1 \
        --pub-key-file ~/.ssh/id_rsa.pub
    # Then play as much as needed:
    secotrec-aws-node show-configuration --path /path/to/store/config-and-more/
    secotrec-aws-node up --path /path/to/store/config-and-more/ [--dry-run]
    secotrec-aws-node down --path /path/to/store/config-and-more/ [--dry-run]
    secotrec-aws-node ssh --path /path/to/store/config-and-more/

Everything should be idempotent (but some “Waiting for” functions may timeout
for now).

More to come…

