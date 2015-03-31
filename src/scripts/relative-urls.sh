#!/bin/sh

grep -rli 'http://jmlr.org' **/*.bib | xargs -I @ sed -i.bak 's/http:\/\/jmlr.org\/proceedings\/papers//g' @
