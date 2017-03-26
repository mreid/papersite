# Ruby Code

This code is for creating `jekyll` sites for hosting on `GitHub pages`

See also: https://github.com/mlresearch/mlresearch.github.io/blob/master/create_volume.sh

Which needs to be modified for inclusion with this script (it currently creates volumes from the existing JMLR pages).

A bit hacky for the moment but in the shell there are various ruby scripts to run. The main code is found in `mlresearch.rb`. Then you can use:

```bash
./update_config.rb NN
```

where `NN` is the volume number that will create/update the `_config.yml`. E.g. for volume 1 it would be created from `v1.bib`. 

To update/create the individual paper information, which is stored in  `_posts` as files with `YYYY-MM-DD-keyname.md` (with the date of publication and keyname from the bibtex entry) use

```bash
./update_papers.rb NN
```

Finally, to copy across the jekyll formatting files from this repo (like layouts, includes, index etc) you simply run

```bash
./update_papersite_files.rb
```

Formatting is done in the file `index.html` in the new repo and the file `_layouts/inproceedings.html`, then there are various includes to get other parts running. 
