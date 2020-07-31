# Ruby Code

This code is for creating `jekyll` sites for hosting PMLR on `GitHub pages`

From the shell there are various ruby scripts to run. The main code
that does the work is found in `mlresearch.rb`. 

## Requirements

The `papersite` script depends on the following packages, which will need to
be install before the scripts here can run:

 - ActiveRecord
 - bibtex-ruby
 - facets
 - pandoc-ruby

You can install all the above with:
```
sudo gem install bibtex-ruby facets pandoc-ruby activerecord
```

## At First Request

When the volume was first requested, a new repo should be set up under

https://github.com/mlresearch/

with the name vNN where NN is the number of the proceedings.

Use this link:

https://github.com/organizations/mlresearch/repositories/new

To set up the proceedings.

Do *not* initialize with a README. Simply "Create Repository" and *then*
add relevant people to the edit/write permissions. 

The "Description" field for the repo should be of the form "AISTATS 2017
Proceedings" or equivalent. This will populate the front page when the
proceedings are available (see http://proceedings.mlr.press/).

## When the Proceedings are Ready

The proceedings editor needs to provide you you with a zipped file that
contains the PDFS and supplemental information, as well as a bib file
containing information about the volume. The specification for all
this is given here:

http://proceedings.mlr.press/spec.html

The scripts run in `papersite`. As a suggested directory structure, if
`papersite` is located at

```bash
~/mlresearch/papersite
```
then create the new directory

```
mkdir ~/mlresearch/vNN
```
with NN being the volume number.

Then unzip the file containing PDFs and the bib file into the new
directory.

```bash
cd ~/mlresearch/vNN
../papersite/ruby/create_volume.rb NN FILE.bib
```

Where NN is the volume number and FILE.bib is the file provided by the
proceedings editors (e.g. gpip2006.bib).

This will move the PDFs into the correct positions, along with
supplemental material.

The site is created using Jekyll. There is a customised remote-theme for formating the proceedings which is referenced in the `_config.yml` file. 

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

