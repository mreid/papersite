#!/bin/sh

rsync --checksum -avz --exclude '*.tgz' --exclude '*.pdf' --exclude '*.zip' --exclude '*.gz' --rsh='ssh -p1022' --no-group /tmp/jmlr/* confla@mark.reid.name:/home/confla/www/pmlr.cc/
