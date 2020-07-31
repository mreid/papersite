#!/usr/bin/env ruby
# File for updating _posts directory with papers for a new proceedings from a directory of bib files.
# Usage: ./update_papersite_files.rb

require 'fileutils'
require_relative 'mlresearch'

print "Updating files. Base directory is " + MLResearch.basedir + "\n"

FileUtils.cp MLResearch.procdir +  'papersite/ruby/index.html', '.'
# Moving to remote repository, these files are now stored there.
#FileUtils.cp MLResearch.procdir +  'papersite/ruby/feed.xml', '.'
#FileUtils.cp MLResearch.procdir +  'papersite/ruby/bibliography.bib', '.'
#FileUtils.cp MLResearch.procdir +  'papersite/ruby/citeproc.yaml', '.'

# directory_name = "_layouts"
# Dir.mkdir(directory_name) unless File.exists?(directory_name)
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_layouts/inproceedings.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_layouts/default.html', directory_name + '/'

# directory_name = "_includes"
# Dir.mkdir(directory_name) unless File.exists?(directory_name)
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/paper_list.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/listpaper.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/listperson.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/head.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/header.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/footer.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/edit_link.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/mathjax_code.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/paper_abstract.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/google_tracking_code.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/paper_google_scholar.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/paper_twitter_meta.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/paper_open_graph_meta.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/hidden_copy_code.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/copy_buttons.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/disqus_comment_code.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/bibtex_copy_section.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/bibtex_entry', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/endnote_copy_section.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/endnote_entry', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/apa_copy_section.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/apa_entry', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/ris_copy_section.html', directory_name + '/'
# FileUtils.cp MLResearch.procdir +  'papersite/ruby/_includes/ris_entry', directory_name + '/'

