#!/bin/sh
{
    set -ex
    # temporarily add dist to deploy to dokku
    git add -f dist
    git commit -m "temp commit for deploy $CIRCLE_SHA1"
    git push dokku -f master
} || {
    git reset HEAD~1
    exit -1
}
git reset HEAD~1
