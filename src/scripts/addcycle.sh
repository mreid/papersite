#!/bin/bash
# Adds a cycle-1 section to all bib files, making backups with ext .bak
for bib in *.bib; do sed -i.bak -e $'2i\\\n\  section = {cycle-1},\n' $bib; done