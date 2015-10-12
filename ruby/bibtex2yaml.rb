#!/usr/bin/env ruby

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'pandoc-ruby'

bibdir = '/Users/neil/mlresearch/papersite/db/'
procdir = '/Users/neil/mlresearch/'
url = 'http://mlr.press'
email = ''
twitter = 'mlresearch'
def detex(text)
  # Returning up to second end character is to deal with new line
  return PandocRuby.convert(text, {:from => :latex, :to => :markdown}, 'no-wrap')[0..-2]
end
def bibtohash(obj, bib)
  # Takes an bib file object and returns a cleaned up hash.
  # Params:
  # +obj+:: Object to clean up
  # +bib+:: +BibTeX+ object that contains strings etc
  # +errhandler+:: +Proc+ object that takes a pipe object as first and only param (may be nil)
  ha = obj.to_hash(:quotes=>'').rekey!(&:to_s)

  ha['layout'] = ha['bibtex_type'].to_s
  ha.tap { |hs| hs.delete('bibtex_type') }

  ha['key'] = ha['bibtex_key']
  ha.tap { |hs| hs.delete('bibtex_key') }

  ha['categories'] = Array.new(1)
  ha['categories'][0] = ha['key']

  ha['month'] = ha['month_numeric']
  ha.tap { |hs| hs.delete('month_numeric') }

  ha.delete_if {|key, value| key[0..2] == "opt" }

  if ha['abstract'] == ''
    ha.tap { |hs| hs.delete('abstract') }
  else
    ha['abstract'] = detex(ha['abstract'])
  end

  if ha.has_key?('pages')
    pages = ha['pages'].split('-')
    ha['firstpage'] = pages[0]
    ha['lastpage'] = pages[-1]
    ha.tap { |hs| hs.delete('pages') }
  end

  if ha.has_key?('sections')
    sections = ha['sections'].split('|')
    hasections = Array.new(sections.length)
    sections.each.with_index do |section, index|
      name_title = section.split('=')
      hasections[index] = {'name' => name_title[0], 'title' => name_title[-1]}
    end
  end
  ha.each do |key, value|
    ha[key] = detex(value)
  end
  ha['sections'] = hasections
  if ha.has_key?('editor')    
    ha['editors'] = splitauthors(ha, obj, type=:editor)
    ha.tap { |hs| hs.delete('editor') }
  end
  
  if ha.has_key?('author')
    ha['authors'] = splitauthors(ha, obj)
    ha.tap { |hs| hs.delete('author') }
  end
  return ha
end

def mindigit(str, num=2)
  str.gsub(/-[0-9]+/, '')
  while str.length < num
    str = '0' + str
  end
  return str
end

def filename(date, title)
  
  f = date + '-' + title + '.md'
  return f
end

def splitauthors(ha, obj, type=:author)
  a = Array.new(obj[type].length)       #=> [nil, nil, nil]
  obj[type].each.with_index(0) do |name, index|
    first = detex(name.first)
    last = detex(name.last)
    a[index] = {'firstname' => first, 'lastname' => last}
  end
  return a
end

if ARGV.length < 1
  puts "Usage: #{$0} <volume>"
else
  volume = ARGV[0]
  proceedings = volume + '.bib'
  bib = BibTeX.open(bibdir + proceedings)
  obj = bib['@proceedings'][0]
  obj.replace(bib.q('@string'))
  obj.join
  ha = bibtohash(obj, bib)
  
  reponame = ha['shortname'].downcase + ha['year']
  system "jekyll new " + procdir + reponame
  File.delete(*Dir.glob(procdir + reponame + '/_posts/*.markdown'))
  # Add details to _config.yml file
  ha['title'] = ha['booktitle']
  ha['conference'] = ha['booktitle']
  ha['email'] = email
  ha['description'] = ''
  ha['url'] = url
  ha['baseurl'] = '/' + reponame
  ha['twitter_username'] = twitter
  ha['github_username'] = 'mlresearch'
  ha['markdown'] = 'kramdown'
  ha['permalink'] = '/:categories/:title.html'
  ya = ha.to_yaml
  published = ha['published']
  out = File.open(procdir + reponame + '/' + '_config.yml', 'w')    
  out.puts "# Site settings"
  out.puts "# Auto generated from " + proceedings
  out.puts ya
end
puts bibdir
puts volume
puts bibdir + volume
Dir.glob(bibdir + volume + '/*.bib') do |bib_file|
  bib = BibTeX.open(bib_file)
  # do work on files ending in .rb in the desired directory
  bib['@inproceedings'].each do |obj|
    obj.replace(bib.q('@string'))
    obj.join
    ha = bibtohash(obj, bib)
    ya = ha.to_yaml
    fname = filename(published, ha['key'])
    out = File.open(procdir + reponame + '/_posts/' + fname, 'w')
    out.puts ya
    out.puts "---"
  end  
end

FileUtils.cp procdir +  'papersite/ruby/inproceedings.html', procdir + reponame + '/_layouts/'
FileUtils.cp procdir + 'papersite/ruby/about.md', procdir + reponame + '/'
FileUtils.cp procdir + 'papersite/ruby/index.html', procdir + reponame + '/'
