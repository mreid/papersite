#!/bin/bash
git init
git checkout -b gh-pages
git add .
git remote add origin git@github.com/mlresearch/$1.git
git remote -v
git commit -a -m "Add $1 pages"
git push --set-upstream origin gh-pages
