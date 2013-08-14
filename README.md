papersite
=========

A Hakyll-based proceedings site generating tool.

## Installation

There are two main programs that are used to manage the JMLR W&CP web site:

- `site`: The HTML generation tool (based on Hakyll).
- `import`: The recevied BibTeX to database import tool.

These are both built by running

	$ cabal configure
	$ cabal build

The configure step will check whether dependencies are present and, if necessary,
download them.

The executables, once built, are at `dist/build/site/site` and 
`dist/build/import/import`. For convenience, I recommend setting up links to these,
for example:

	$ ln -s dist/build/site/site ./site
	$ ln -s dist/build/import/import ./import


### Managing Haskell Environments

_Note: This is not needed unless you are developing on more than one branch of this software_.

As various Haskell libraries change over time, it is sometimes necessary to upgrade
the site generation tools but be able to use older versions to generate and do
regression tests.

To manage this, I've opted to use [hsenv](https://github.com/tmhedberg/hsenv) as
follows:

	$ cabal install hsenv
	$ cd ~/Library/Haskell
	$ hsenv --name hakyll4.2    # To run against older Hakyll build
	$ source ~/Library/Haskell/.hsenv_hakyll4.2/bin/activate
	(A new environment called hakyll4.2 is activated)
	
	$ cabal install hakyll-4.2.2.0
	(Hakyll 4.2.2.0, its dependencies, and others are built in hakyll4.2 environment)

	$ cd ~/path/to/papersite
	$ cabal build 
	(Gets dependencies for `site` and builds it)

Now the papersite applications will be built against Hakyll 4.2 and be available
in the directory `dist_hakyll4.2/build`.

To switch back to your system's usual Haskell build and libraries use

	$ deactivate_hsenv

## Usage

Using these tools occurs in two steps:

1. Importing the BibTeX file and PDFs received from a conference organiser into
   a simple, file-based database.

2. Generating the HTML from the files imported into the database.

The beauty of the file-based database is that, once the information for a conference
is imported, the information for individual papers can be edited and managed under 
version control. After each change, the HTML for the site can be (incrementally)
updated and deployed.

### Importing

To import a BibTeX file received from a conference organiser (say `conf13.bib`)
into the directory `db`:

	$ ./import conf13.bib db

This will create a file-system-based database in the directory in `db`. 
If `NN` is the value of the `volume` field in the `@proceedings` entry in the BibTeX
file, it will consists of:

- A BibTeX file `vNN.bib` with the conference metadata.
- A directory `vNN` with files of the form `ID.bib`, `ID.pdf`, and `ID-supp.*`.
  Each of these files correspond to the keys in the `@InProceedings` entries in 
  `conf13.bib`.

### Generating the site

To generate the sites for all the proceedings in the `db` directory, run:

	$ ./site rebuild

This will create a static site in `/tmp/jmlr`. This site can be viewed by 
running:

	$ ./site preview

and then pointing a browser at `http://localhost:8000/`.

If changes are made to entries, just run `./site build` to update without rebuilding
the entire site.

### Deploying the site

Once you are happy with how the site looks, it can be pushed to the JMLR.org by
running:

	$ ./site deploy

That's it!

You may need to update the credentials to those you use to log into the CSAIL
web server. To do this, find the definition of `config` in `site.hs` and edit
the string containing `mreid@login.csail.mit.edu` to your username.

Once you've made this edit, run `cabal build` to rebuild the `site` tool.

## Proceedings BibTeX File Format

The entire site for a conference is generated from a single BibTeX file. 
Because the site generation is automated, it is important that the BibTeX file
conform exactly to the following:

1. Each paper in the proceedings must appear in the file as an 
   `@InProceedings` entry. Each entry **must** have the following fields:

   - `title`: The title of the paper.
   - `abstract`: The paper's abstract (in valid LaTeX).
   - `author`:   The paper's authors in Lastname, Firstnames format, separated by
                 `and`.
   - `pages`:    The page numbers for the paper in _startpage_ - _endpage_ format.

2. A `@Conference` entry must appear in the file with the conference details.
   The following fields **must** be present:

   - `editor`: The editors' names (in same format as `authors` above).
   - `title`:  The title of the conference.
   - `year`:   The year of the conference.
   - `volume`: The assigned JMLR W&CP volume number. 

	The following fields are optional:
   - `booktitle`: e.g., `Proceedings of the Nth Conference on Stuff`.
   - `month`:     The month of the conference.

**Note**: All field values in the BibTeX file must be valid BibTeX. For example:
 - Names with accents (e.g., _François_) must be written in LaTeX 
   (e.g., `Fran\c{c}ois`). 
 - Any mathematics in the title or abstract must be surrounded by matching `$`.
 - LaTeX ``quotation marks'' must be used.
