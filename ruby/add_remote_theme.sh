#!/bin/bash

git pull
../papersite/ruby/update_volume.rb
rm -rf _layouts/ _includes/ bibliography.bib feed.xml citeproc.yaml 
git add README.md Gemfile
git commit -a -m "Update with remote theme"
git push

