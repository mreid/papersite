papersite
=========

A Hakyll-based proceedings site generating tool

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
   - `pages`:    The page numbers for the paper in _startpage_-_endpage_ format.

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
