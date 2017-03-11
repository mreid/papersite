#!/usr/bin/env ruby

require 'fileutils'
require_relative 'mlresearch'

FileUtils.cp MLResearch.procdir +  'papersite/ruby/index.html', '.'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/feed.xml', '.'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/bibliography.bib', '.'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/citeproc.yaml', '.'

directory_name = "_layouts"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
FileUtils.cp MLResearch.procdir +  'papersite/ruby/inproceedings.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/default.html', directory_name + '/'

directory_name = "_includes"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
FileUtils.cp MLResearch.procdir +  'papersite/ruby/paper_list.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/listpaper.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/listperson.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/head.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/header.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/footer.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/edit_link.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/mathjax_code.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/paper_abstract.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/google_tracking_code.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/paper_google_scholar.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/paper_twitter_meta.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/paper_open_graph_meta.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/hidden_copy_code.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/copy_buttons.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/disqus_comment_code.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/bibtex_copy_section.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/bibtex_entry', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/endnote_copy_section.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/endnote_entry', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/apa_copy_section.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/apa_entry', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/ris_copy_section.html', directory_name + '/'
FileUtils.cp MLResearch.procdir +  'papersite/ruby/ris_entry', directory_name + '/'

