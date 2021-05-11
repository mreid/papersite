#!/usr/bin/env ruby

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'
require_relative 'mlresearch'

require 'optparse'

procdir = '/Users/neil/mlresearch/'

volume_no = nil
volume_type = nil
volume_prefix = ''
bib_file=nil
video_file=nil
software_file=nil
reponame=nil
supp_file = nil
supp_name = nil

OptionParser.new do |parser|
  parser.banner = "Usage: create_volume.rb -v VOLUME -b BIBFILE [optional]"
  parser.on("-v", "--volume VOLUME", Integer,
            "Write the specific VOLUME of PMLR") do |number|
    volume_no=number
    volume_type="Volume"
    volume_previx = "v"
  end
  parser.on("-r", "--reissue VOLUME", Integer,
            "Write the specific VOLUME of PMLR") do |number|
    volume_no=number
    volume_type="Reissue"
    volume_prefix = "r"
  end
  parser.on("-b", "--bibfile BIBFILE", String,
            "The bib file containing information about the papers") do |filename|
    bib_file=filename
  end
  parser.on("-s", "--software-file filename", String,
            "A csv file containing information about software links") do |filename|
    software_file=filename
  end
  parser.on("-V", "--video-file filename", String,
            "A csv file containing information about video links") do |filename|
    video_file=filename
  end
  parser.on("-S", "--supplementary-file filename", String,
            "A csv file containing information about supplementary links") do |filename|
    supp_file=filename
    supp_name = 'Supplementary Material'
  end
  parser.on("-l", "--label supplementary_label", String,
            "A csv file containing information about supplementary label") do |label|
    supp_name=label
  end

end.parse!

reponame = volume_prefix + volume_no.to_s
if not supp_file.nil?
  supp_file = MLResearch.procdir + reponame + '/' + supp_file
end
if not video_file.nil?
  video_file = MLResearch.procdir + reponame + '/' + video_file
end
if not bib_file.nil?
  bib_file = MLResearch.procdir + reponame + '/' + bib_file
end
if not software_file.nil?
  software_file = MLResearch.procdir + reponame + '/' + software_file
end


# Write the _config.yml file
volume_info = MLResearch.bibextractconfig(bib_file, volume_no, volume_type)
MLResearch.write_volume_files(volume_info)

# Write the papers
directory_name = "_posts"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
MLResearch.extractpapers(bib_file, volume_no, volume_info, software_file, video_file, supp_file, supp_name)  
out = File.open('index.html', 'w')
out.puts "---"
out.puts "layout: home"
out.puts "---"

