#!/usr/bin/env ruby

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'
PandocRuby.pandoc_path = '/usr/local/bin/pandoc'
bibdir = '/Users/neil/mlresearch/papersite/db/'
procdir = '/Users/neil/mlresearch/'
url = 'http://proceedings.mlr.press'
email = ''
twitter = 'mlresearch'

class String
    def is_i?
       /\A[-+]?\d+\z/ === self
    end
end

def detex(text)
  # Returning up to second end character is to deal with new line
  return LaTeX.decode text
end

#def detex_abstract(text)
#  return PandocRuby.convert(text, {:from => :latex, :to => :markdown}, 'no-wrap')[0..-2]
#end

def bibtohash(obj, bib)
  # Takes an bib file object and returns a cleaned up hash.
  # Params:
  # +obj+:: Object to clean up
  # +bib+:: +BibTeX+ object that contains strings etc
  # +errhandler+:: +Proc+ object that takes a pipe object as first and only param (may be nil)
  ha = obj.to_hash(:quotes=>'').rekey!(&:to_s)
  ha['layout'] = ha['bibtex_type'].to_s
  ha.tap { |hs| hs.delete('bibtex_type') }
  ha['series'] = "Proceedings of Machine Learning Research"
  ha['id'] = ha['bibtex_key'].to_s
  ha.tap { |hs| hs.delete('bibtex_key') }

  #ha['categories'] = Array.new(1)
  #ha['categories'][0] = ha['key']

  ha['month'] = ha['month_numeric'].to_i
  ha.tap { |hs| hs.delete('month_numeric') }

  ha.delete_if {|key, value| key[0..2] == "opt" }

  if ha['abstract'] == ''
    ha.tap { |hs| hs.delete('abstract') }
  else
    ha['abstract'] = detex(ha['abstract'])
  end
  ha['tex_title'] = ha['title']
  ha['title'] = detex(ha['title'])

  if ha.has_key?('pages')
    pages = ha['pages'].split('-')
    if pages[0].is_i?
      ha['firstpage'] = pages[0].to_i
    else
      ha['firstpage'] = pages[0]
    end
    if pages[-1].is_i?
      ha['lastpage'] = pages[-1].to_i
    else
      ha['lastpage'] = pages[-1]
    end
    ha['page'] = ha['firstpage'].to_s + '-' + ha['lastpage'].to_s
    ha.tap { |hs| hs.delete('pages') }
  end
  if ha.has_key?('firstpage')
    ha['order'] = ha['firstpage'].to_i
  end

  published = ha['published']
  ha['cycles'] = false
  if ha.has_key?('sections')
    sections = ha['sections'].split('|')
    hasections = Array.new(sections.length)
    section_dates = ha['published'].split('|')
    sections.each.with_index do |section, index|
      name_title = section.split('=')
      if(section_dates.length==hasections.length)
        date = Date.parse section_dates[index]
        hasections[index] = {'name' => name_title[0], 'title' => name_title[-1], 'published' => date}
        ha['cycles']= true
      else
        hasections[index] = {'name' => name_title[0], 'title' => name_title[-1]}
      end
    end
    ha['sections'] = hasections
  end
    
  if ha.has_key?('editor')
    editor = splitauthors(ha, obj, type=:editor)
    ha.tap { |hs| hs.delete('editor') }
    ha['editor'] = editor
  end
  
  if ha.has_key?('author')
    author = splitauthors(ha, obj)
    ha.tap { |hs| hs.delete('author') }
    ha['author'] = author
  end
  if ha.has_key?('published')
    ha['published'] = Date.parse ha['published']
  else
    #ha['date'] = Date.parse "0000-00-00 00:00:00"
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
  puts title
  f = date.to_s + '-' + title.to_s + '.md'
  return f
end

def splitauthors(ha, obj, type=:author)
  a = Array.new(obj[type].length)       #=> [nil, nil, nil]
  obj[type].each.with_index(0) do |name, index|
    first = detex(name.first)
    last = detex(name.last)
    a[index] = {'given' => first, 'family' => last}
  end
  return a
end

if ARGV.length < 1
  puts "Usage: #{$0} <volume>"
else
  volume = ARGV[0]
  reponame = 'v' + volume.to_s

  proceedings = 'v' + volume.to_s + '.bib'
  puts bibdir + proceedings
  bib = BibTeX.open(bibdir + proceedings)
  obj = bib['@proceedings'][0]
  obj.replace(bib.q('@string'))
  obj.join
  ha = bibtohash(obj, bib)
  ha['title'] = "Proceedings of Machine Learning Research"
  booktitle = ha['booktitle']
  ha['description'] = booktitle
  if ha.has_key?('address')
    ha['description'] += "\n  Held in " + ha['address'] 
  end
  if ha.has_key?('start') and ha.has_key?('end')
    ha['description'] += " on "
    if (ha['start'].year == ha['end'].year) and (ha['start'].month == ha['end'].month)
      if (ha['start'].day == ha['end'].day)
        ha['description'] += "#{ha['end'].strftime('%d %B %Y')}"
        ha['date_str'] = "#{ha['end'].strftime('%d %b')}"
      else
        ha['description'] += "#{ha['start'].strftime('%d')}-#{ha['end'].strftime('%d %B %Y')}"
        ha['date_str'] = "#{ha['start'].strftime('%d')}--#{ha['end'].strftime('%d %b')}"
      end
    else
      ha['description'] += "#{ha['start'].strftime('%d %B')} to #{ha['end'].strftime('%d %B %Y')}"
      ha['date_str'] = "#{ha['start'].strftime('%d %b')}--#{ha['end'].strftime('%d %b')}"
    end
  end
  if(ha['cycles'])
    ha['description'] += "\n\nPublished in #{ha['sections'].length} Sections as Volume " + volume.to_s + " by the Proceedings of Machine Learning Research.\n"
    ha['sections'].each.with_index(0) do |section, index|
      ha['description'] += "  #{section['title']} published on #{section['published'].strftime('%d %B %Y')}\n"
    end
  else
    ha['description'] += "\n\nPublished as Volume " + volume.to_s + " by the Proceedings of Machine Learning Research on #{ha['published'].strftime('%d %B %Y')}." + "\n"
  end
  if ha.has_key?('editor')
    ha['description'] += "\nVolume Edited by:\n"
    for name in ha['editor'] 
      ha['description'] += "  #{name['given']} #{name['family']}\n"
    end
  end
  ha['description'] += "\nSeries Editors:\n  Neil D. Lawrence\n"
  if (volume.to_i>27)
    ha['description'] += "  Mark Reid\n"
  end
  ha['url'] = url
  ha['baseurl'] = '/' + reponame
  ha['twitter_username'] = twitter
  ha['github_username'] = 'mlresearch'
  ha['markdown'] = 'kramdown'
  ha['permalink'] = '/:title.html'
  ha['github'] = {'edit' => true, 'repository' => reponame}
  if not ha.has_key?('name')
    ha['name'] = booktitle
  end
  ha['display'] = {'copy_button' => {'bibtex' => true, 'endnote' => true, 'apa' => true}}
  if ha.has_key?('comments')
    if ha['comments'].downcase == 'yes' or ha['comments'].downcase == 'true'
      ha['display']['comments'] = true
    else
      ha['display']['comments'] = false
    end
  else
    ha['display']['comments'] = false
  end

  #reponame = ha['shortname'].to_s.downcase + ha['year'].to_s
  #system "jekyll new " + procdir + reponame
  #File.delete(*Dir.glob(procdir + reponame + '/_posts/*.markdown'))
  # Add details to _config.yml file
  ha['volume'] = volume.to_i
  ha['email'] = email
  address = ha['address']
  ha['conference'] = {'name' => ha['name'], 'url' => ha['conference_url'], 'location' => address, 'dates'=>ha['start'].upto(ha['end']).collect{ |i| i}}
  ha.tap { |hs| hs.delete('address') }
  ha.tap { |hs| hs.delete('conference_url') }
  ha.tap { |hs| hs.delete('name') }
  
  ha['analytics'] = {'google' => {'tracking_id' => 'UA-92432422-1'}}
  ya = ha.to_yaml(:ExplicitTypes => true)
 
  out = File.open('_config.yml', 'w')    
  out.puts ya
  out.puts "# Site settings"
  out.puts "# Auto generated from " + proceedings
end
volume_info = ha
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
    ha['date'] = volume_info['published']
    published = ha['date']
    if ha.has_key?('section')
      if volume_info.has_key?('sections')
        volume_info['sections'].each_with_index do |item, index|
          if ha['section'] == item['name']
            if item.has_key?('published')
              published = item['published']
              ha['date'] = item['published']
              ha['number'] = index + 1
            end
          end
        end
      end
    end

    ha['address'] = address
    ha['publisher'] = 'PMLR'
    ha['container-title'] = booktitle
    ha['volume'] = volume.to_s
    ha['genre'] = 'inproceedings'
    ha['issued'] = {'date-parts' => [published.year, published.month, published.day]}
    if volume.to_i>27
      ha['pdf'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + ha['id'] + '.pdf'
    else
      ha['pdf'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + ha['id'] + '/' + ha['id'] + '.pdf'
    end
    if ha.has_key?('supplementary')
      supple = ha['supplementary'].split(':')[-1]
      if volume.to_i>27
        ha['supplementary'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + supple
      else
        ha['supplementary'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + ha['id'] + '/' + supple
      end
    end
    
    
    ya = ha.to_yaml(:ExplicitTypes => true)
    fname = filename(published, ha['id'])
    out = File.open('_posts/' + fname, 'w')
    out.puts ya
    out.puts "# Format based on citeproc: http://blog.martinfenner.org/2013/07/30/citeproc-yaml-for-bibliographies/"
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
FileUtils.cp procdir +  'papersite/ruby/listpaper.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/listperson.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/head.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/header.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/footer.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/edit_link.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/mathjax_code.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/paper_abstract.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/google_tracking_code.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/paper_google_scholar.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/hidden_copy_code.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/disqus_comment_code.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/bibtex_copy_section.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/bibtex_entry', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/endnote_copy_section.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/endnote_entry', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/apa_copy_section.html', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/apa_entry', '_includes/'
FileUtils.cp procdir +  'papersite/ruby/index.html', '.'
FileUtils.cp procdir +  'papersite/ruby/feed.xml', '.'
FileUtils.cp procdir +  'papersite/ruby/bibliography.bib', '.'
FileUtils.cp procdir +  'papersite/ruby/citeproc.yaml', '.'
