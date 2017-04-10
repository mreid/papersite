# Ruby Code

This code is for creating `jekyll` sites for hosting on `GitHub pages`


A bit hacky for the moment but in the shell there are various ruby scripts to run. The main code is found in `mlresearch.rb`. 

Unzip the file containing PDFs and the bib file into the current
directory.

Then run

```bash
../papersite/ruby/create_volume.rb VV FILE.bib
```

Where VV is the volume number and FILE.bib is the file provided by the
proceedings editors (e.g. gpip2006.bib).

This will move the PDFs into the correct positions, alongwith
supplemental material.

Then run

```bash
../papersite/ruby/update_papersite_files.rb NN
```

To bring the relevant paper site files across. Formatting is done in the file `index.html` in the new repo and the file `_layouts/inproceedings.html`, then there are various includes to get other parts running. 

Now, assuming you have already created the stub git repo on github,
you can run the script
```bash
# CAREFUL RUNNING THIS! MAKE SURE YOU GET THE REPO VOLUME RIGHT!
../papersite/ruby/addrepo.sh NN
```
which will init the git repo, and check in the code to the relevant
repo. 


## Old JMLR Pages

These scripts were also used to copy across from the old JMLR pages,
see: https://github.com/mlresearch/mlresearch.github.io/blob/master/create_volume.sh

Other files for updating from the original `db` format can be found as
below:

```bash
./update_config.rb NN
```

To update/create the individual paper information from the
`papersite/db/` storage  (stored in  `_posts` as files with `YYYY-MM-DD-keyname.md` with the date of publication and keyname from the bibtex entry) use

```bash
./update_papers.rb NN
```

