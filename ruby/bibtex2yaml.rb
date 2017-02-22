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
  #return PandocRuby.convert(text, {:from => :latex, :to => :markdown}, 'no-wrap')[0..-2]
  return text
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

  ha['key'] = ha['bibtex_key'].to_s
  ha.tap { |hs| hs.delete('bibtex_key') }

  ha['categories'] = Array.new(1)
  ha['categories'][0] = ha['key']

  ha['month'] = ha['month_numeric'].to_i
  ha.tap { |hs| hs.delete('month_numeric') }

  ha.delete_if {|key, value| key[0..2] == "opt" }

  if ha['abstract'] == ''
    ha.tap { |hs| hs.delete('abstract') }
  else
    ha['abstract'] = detex(ha['abstract'])
  end

  
  if ha.has_key?('pages')
    pages = ha['pages'].split('-')
    ha['firstpage'] = pages[0].to_i
    ha['lastpage'] = pages[-1].to_i
    puts ha['firstpage'] + ha['lastpage'] 
    ha.tap { |hs| hs.delete('pages') }
  end

  if ha.has_key?('comments')
    if ha['comments'].downcase == 'yes' or ha['comments'].downcase == 'true'
      ha['comments'] = true
    end
  end
  
  if ha.has_key?('sections')
    sections = ha['sections'].split('|')
    hasections = Array.new(sections.length)
    sections.each.with_index do |section, index|
      name_title = section.split('=')
      hasections[index] = {'name' => name_title[0], 'title' => name_title[-1]}
    end
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
  if ha.has_key?('published')
    ha['published'] = Date.parse ha['published']
  end
  if ha.has_key?('start')
    ha['start'] = Date.parse ha['start']
  end
  if ha.has_key?('end')
    ha['end'] = Date.parse ha['end']
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
  puts date
  puts title
  f = date.to_s + '-' + title.to_s + '.md'
  return f
end

def splitauthors(ha, obj, type=:author)
  a = Array.new(obj[type].length)       #=> [nil, nil, nil]
  obj[type].each.with_index(0) do |name, index|
    first = detex(name.first)
    last = detex(name.last)
    a[index] = {'givenname' => first, 'surname' => last}
  end
  return a
end

if ARGV.length < 1
  puts "Usage: #{$0} <volume>"
else
  volume = ARGV[0]
  proceedings = 'v' + volume.to_s + '.bib'
  puts bibdir + proceedings
  bib = BibTeX.open(bibdir + proceedings)
  obj = bib['@proceedings'][0]
  obj.replace(bib.q('@string'))
  obj.join
  ha = bibtohash(obj, bib)

  reponame = 'v' + volume.to_s
  #reponame = ha['shortname'].to_s.downcase + ha['year'].to_s
  #system "jekyll new " + procdir + reponame
  #File.delete(*Dir.glob(procdir + reponame + '/_posts/*.markdown'))
  # Add details to _config.yml file
  ha['title'] = ha['booktitle']
  ha['volume'] = volume.to_s
  ha['conference'] = ha['booktitle']
  ha['email'] = email
  ha['description'] = ha['booktitle']
  if ha.has_key?('address')
    ha['description'] += ', ' + ha['address']
  end
  ha['url'] = url
  ha['baseurl'] = '/' + reponame
  ha['twitter_username'] = twitter
  ha['github_username'] = 'mlresearch'
  ha['markdown'] = 'kramdown'
  ha['permalink'] = '/:categories/:title.html'
  ya = ha.to_yaml(:ExplicitTypes => true)
  published = ha['published']
  
  out = File.open('_config.yml', 'w')    
  out.puts "# Site settings"
  out.puts "# Auto generated from " + proceedings
  out.puts ya
end
puts bibdir
puts volume.to_s
puts bibdir + 'v' +  volume.to_s
directory_name = "_posts"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
Dir.glob(bibdir + 'v' + volume.to_s + '/*.bib') do |bib_file|
  bib = BibTeX.open(bib_file)
  # do work on files ending in .rb in the desired directory
  bib['@inproceedings'].each do |obj|
    obj.replace(bib.q('@string'))
    obj.join
    ha = bibtohash(obj, bib)
    ya = ha.to_yaml(:ExplicitTypes => true)
    fname = filename(published, ha['key'])
    out = File.open('_posts/' + fname, 'w')
    out.puts ya
    out.puts "---"
  end  
end
directory_name = "_layouts"
Dir.mkdir(directory_name) unless File.exists?(directory_name)
directory_name = "_includes"
Dir.mkdir(directory_name) unless File.exists?(directory_name)

FileUtils.cp procdir +  'papersite/ruby/inproceedings.html', '_layouts/'
FileUtils.cp procdir +  'papersite/ruby/default.html', '_layouts/'
FileUtils.cp procdir +  'papersite/ruby/paper_list.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/head.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/header.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/footer.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/paper_abstract.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/paper_google_scholar.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/disqus_comment_code.html', '_includes/'
FileUtils.cp procdir + 'papersite/ruby/about2.md', '.'
FileUtils.cp procdir + 'papersite/ruby/index2.html', '.'
