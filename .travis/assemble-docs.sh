#!/bin/bash

# Copy haddocks to a separate directory.
cp -R "$(stack path --local-doc-root)" ../gh-pages
cp -R "$(stack path --local-hpc-root)" ../gh-pages
cd ../gh-pages

# Set identity.
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

# Add branch.
git init
git remote add origin https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git > /dev/null
git checkout -B gh-pages

# Push generated files.
git add .
git commit -m "Haddocks updated"
git push origin gh-pages -fq > /dev/null
