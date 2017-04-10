#!/usr/bin/env ruby

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'
require 'mlresearch'

PandocRuby.pandoc_path = '/usr/local/bin/pandoc'
bibdir = '/Users/neil/mlresearch/papersite/db/'
procdir = '/Users/neil/mlresearch/'
url = 'http://proceedings.mlr.press'
email = ''
twitter = 'mlresearch'

volume_info = ha


puts bibdir
puts volume.to_s
puts bibdir + 'v' +  volume.to_s
directory_name = "_posts"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
Dir.glob(bibdir + 'v' + volume.to_s + '/*.bib') do |bib_file|
  mlresearch.extractpapers(bib_file, volume, volume_info)
end
