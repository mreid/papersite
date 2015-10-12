# Ruby Code

This code is for creating `jekyll` sites for hosting on `GitHub pages`

A bit hacky for the moment but in the shell run

```bash
./bibtex2yaml.rb v1
```

and that will create/update the system for volume 1. That volume is `gpip2006`. The main conference information is stored in `_config.yml` which is created from `v1.bib`. The individual paper information is stored as 'posts' in `_posts` as files with `YYYY-MM-DD-keyname.md` for the date of publication and keyname is the bibtex key name.

Formatting is done in the file `index.html` in the new repo and the file `_layouts/inproceedings.html` in the repo.

For a new conference, once the ruby script has been run, create a repo on github with the `shortnameYYYY` format. Don't create the README file or anything. Then run the script
```bash
./addrepo.sh shortnameYYYY
```
which will init the git repo, and check in the code to the relevant repo. CAREFUL RUNNING THIS! MAKE SURE YOU GET THE REPO NAME RIGHT!

