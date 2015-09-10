#!/bin/bash


set -e

#this is in here so we can override it if we need too.
test_clone_args=" https://github.com/SimVascular/SimVascular-Test.git "
rm -rf $SV_TEST_DIR
git clone $test_clone_args $SV_TEST_DIR

