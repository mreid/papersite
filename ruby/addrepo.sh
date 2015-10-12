git init
git checkout -b gh-pages
git add .
git remote add origin "https://github.com/mlresearch/gpip2006"
git remote -v
git commit -a -m "Add gpip2006 pages"
git push --set-upstream origin gh-pages
