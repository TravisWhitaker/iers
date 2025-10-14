#!/usr/bin/env bash

set -eou pipefail

tar cf ./test.tar ./*.txt
xz -9 ./test.tar
