#!/bin/bash
git init
git checkout -b gh-pages
git add README.md _posts index.html *.bib Gemfile _config.yml
git remote add origin git@github.com/mlresearch/$1.git
git remote -v
git commit -a -m "Add $1 pages"
git push --set-upstream origin gh-pages
for letter in {a..z}
do git add $letter*
   git commit -a -m "Add $1 pages"
   git push
done
