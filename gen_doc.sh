#!/bin/sh
pasdoc -E html_doc -S documented_units --marker=-- --use-tipue-search --visible-members=published,public,protected,private -DPAS_DOC
