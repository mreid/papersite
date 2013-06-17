# To Do

## Bugs

- [X] Do not use Pandoc to render titles for Google citation meta information (cf. Woodruff13 in v30)

- [X] Load only papers for given conference (Snapshotting not working)
      (Search path now built from conference ID)

- [X] Figure out why some entries are not imported correctly.
      (Wasn't closing file handle after writing entry)

## Improvements
- [_] Test for missing PDFs for each BibTeX entry imported.

- [ ] Add javascript to embed discussion on each paper page

- [ ] Improve the docs for the system on GitHub (especially dependencies) 
      so others can run it

- [X] Prettify the pages that are generated

- [X] Add the Google Scholar metadata to each paper page


- [X] Document exactly how we want things like BibTeX entry identifiers to 
      appear and other constraints on the BibTeX file

- [X] Add some code to automate pushing the generated sites to the JMLR server

- [X] Preprocess single BibTeX files into one-per-paper and add link to 
	  each's paper's BibTeX.
