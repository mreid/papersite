#!/bin/bash
# Add explicit absolute PDF links to BibTeX entries in volumes 27-32
for v in v27 v28 v29 v30 v31 v32; do 
	for bib in $v/*.bib; do 
		echo $bib;
		base=`basename -s.bib $bib`
		sed -i.bak -e $'2i\\\n\  pdf = {http://jmlr.org/proceedings/papers/'"$v"'/'"$base"'.pdf},' $bib
	done; 
done
