# To Do

## Bugs

- [_] ID/Link to PDF for `v2/mansinghka07a` should be
      http://jmlr.org/proceedings/papers/v2/masinghka07a/masinghka07a.pdf

### Fixed

- [X] MathJax is evaluating equations in the metadata.

- [X] Empty section title when no section field in top-level .bib

- [X] Supplementary files are always displayed.

- [X] Cannot handle all proceedings simultaneously (too many file handles open)
      Current workaround: move all but latest files into 
	  directory other than `db/`
	  (FIXED: Using Hakyll 4.3 which handles files better)

## Improvements

- [_] Test for missing PDFs for each BibTeX entry imported.

- [_] Test for clashing BibTeX keys upon importing.

- [ ] Add javascript to embed discussion on each paper page

- [ ] Improve the docs for the system on GitHub (especially dependencies) 
      so others can run it


### Completed

- [X] Test cabal install on fresh machine

- [X] Prettify the pages that are generated

- [X] Add the Google Scholar metadata to each paper page

- [X] Document exactly how we want things like BibTeX entry identifiers to 
      appear and other constraints on the BibTeX file

- [X] Add some code to automate pushing the generated sites to the JMLR server

- [X] Preprocess single BibTeX files into one-per-paper and add link to 
	  each's paper's BibTeX.

- [X] Do not use Pandoc to render titles for Google citation meta information (cf. Woodruff13 in v30)

- [X] Load only papers for given conference (Snapshotting not working)
      (Search path now built from conference ID)

- [X] Figure out why some entries are not imported correctly.
      (Wasn't closing file handle after writing entry)
