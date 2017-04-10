# Ruby Code

This code is for creating `jekyll` sites for hosting on `GitHub pages`

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
../papersite/ruby/update_papersite_files.rb VV
```

To bring the relevant paper site files across.

Now, assuming you have already created the stub git repo on github,
you can run the script
```bash
# CAREFUL RUNNING THIS! MAKE SURE YOU GET THE REPO VOLUME RIGHT!
../papersite/ruby/addrepo.sh VV
```
which will init the git repo, and check in the code to the relevant
repo. 

