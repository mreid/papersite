#!/usr/bin/env ruby

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'
require_relative 'mlresearch'

procdir = '/Users/neil/mlresearch/'

if ARGV.length < 2
  puts "Usage: #{$0} <volume> <bibfile>"
else
  volume = ARGV[0]
  bib_filename = ARGV[1]
  reponame = 'v' + volume.to_s
  bib_file = MLResearch.procdir + reponame + '/' + bib_filename
  volume_info = MLResearch.bibextractconfig(bib_file, volume)
  MLResearch.write_volume_files(volume_info)
  directory_name = "_posts"
  Dir.mkdir(directory_name) unless File.exists?(directory_name)
  MLResearch.extractpapers(bib_file, volume, volume_info)  
  out = File.open('index.html', 'w')
  out.puts "---"
  out.puts "layout: home"
  out.puts "---"

end
