require 'rubygems'
require 'bibtex'
require 'yaml'
require 'csv'
require 'facets'

require 'latex/decode'
require 'latex/decode/version'
require 'latex/decode/compatibility'
require 'latex/decode/base'

require 'latex/decode/accents'
require 'latex/decode/diacritics'
require 'latex/decode/maths'
require 'latex/decode/punctuation'
require 'latex/decode/symbols'
require 'latex/decode/greek'

require "active_support/inflector"

require 'fileutils'
require 'pandoc-ruby'


PandocRuby.pandoc_path = '/usr/local/bin/pandoc'
class String
  def is_i?
    /\A[-+]?\d+\z/ === self
  end
end

module MLResearch
  def self.basedir
    # Get base of directory containing `papersite` repo by going three
    # steps up from where this file is located
    File.dirname(__FILE__).split('/')[0..-3].join('/')
  end 
  def self.procdir
    self.basedir + '/'
  end
  def self.bibdir
    self.procdir + '/papersite/db/'
  end
  def self.url
    'http://proceedings.mlr.press'
  end
  def self.tracking_id
    'UA-92432422-1'
  end
  def self.github
    'mlresearch'
  end
  def self.twitter
    'MLResearchPress'
  end
  def self.markdown
    'kramdown'
  end
  def self.publisher
    'Proceedings of Machine Learning Research'
  end
  def self.email
    ''
  end

  def self.detex(string)
    # Returning up to second end character is to deal with new line
    return string unless string.respond_to?(:to_s)
    string = string.is_a?(String) ? string.dup : string.to_s
    string.force_encoding("utf-8")
    LaTeX::Decode::Base.normalize(string)
    LaTeX::Decode::Accents.decode!(string)
    LaTeX::Decode::Diacritics.decode!(string)
    LaTeX::Decode::Punctuation.decode!(string)
    LaTeX::Decode::Symbols.decode!(string)
    LaTeX::Decode::Greek.decode!(string)
    
    LaTeX::Decode::Base.strip_braces(string)

    LaTeX.normalize_C(string)
    # Need to deal with different encodings. Map to utf-8
  end

    def self.detex_abstract(string)
    # Returning up to second end character is to deal with new line
    return string unless string.respond_to?(:to_s)
    string = string.is_a?(String) ? string.dup : string.to_s
    string.force_encoding("utf-8")
    LaTeX::Decode::Base.normalize(string)
    LaTeX::Decode::Accents.decode!(string)
    LaTeX::Decode::Diacritics.decode!(string)
    LaTeX::Decode::Punctuation.decode!(string)
    #LaTeX::Decode::Symbols.decode!(string)
    #LaTeX::Decode::Greek.decode!(string)
    # Don't remove brackets as it messes up maths.
    
    LaTeX.normalize_C(string)
    # Need to deal with different encodings. Map to utf-8
  end

  def self.detex_tex_title(string)
    # Returning up to second end character is to deal with new line
    return string unless string.respond_to?(:to_s)
    string = string.is_a?(String) ? string.dup : string.to_s
    string.force_encoding("utf-8")
    LaTeX::Decode::Base.normalize(string)
    LaTeX::Decode::Accents.decode!(string)
    LaTeX::Decode::Diacritics.decode!(string)
    LaTeX::Decode::Punctuation.decode!(string)
    LaTeX::Decode::Symbols.decode!(string)
    LaTeX::Decode::Greek.decode!(string)    
    LaTeX.normalize_C(string)
    # Need to deal with different encodings. Map to utf-8
  end
  #def self.detex_abstract(text)
  #  return PandocRuby.convert(text, {:from => :latex, :to => :markdown}, 'no-wrap')[0..-2]
  #end
  
  def self.bibtohash(obj)
    # Takes an bib file object and returns a cleaned up hash.
    # Params:
    # +obj+:: Object to clean up
    # +bib+:: +BibTeX+ object that contains strings etc
    # +errhandler+:: +Proc+ object that takes a pipe object as first and only param (may be nil)
    
    ha = obj.to_hash(:quotes=>'').stringify_keys!()
    ha['layout'] = ha['bibtex_type'].to_s
    ha.tap { |hs| hs.delete('bibtex_type') }
    ha['series'] = "Proceedings of Machine Learning Research"
    ha['publisher'] = "PMLR"
    ha['issn'] = '2640-3498'
    ha['id'] = ha['bibtex_key'].to_s
    ha.tap { |hs| hs.delete('bibtex_key') }
    
    #ha['categories'] = Array.new(1)
    #ha['categories'][0] = ha['key']
    
    ha['month'] = ha['month_numeric'].to_i
    ha.tap { |hs| hs.delete('month_numeric') }
    
    ha.delete_if {|key, value| key[0..2] == "opt" }

    if ha.has_key?('abstract')
      if ha['abstract'] == ''
        ha.tap { |hs| hs.delete('abstract') }
      else
        ha['abstract'] = detex_abstract(ha['abstract'])
      end
    end
    if ha.has_key?('title')
      ha['tex_title'] = detex_tex_title(ha['title'])
      ha['title'] = detex(ha['title'])
    end
    if ha.has_key?('pages')
      pages = ha['pages'].split('-')
      pages[0] = pages[0].strip
      pages[-1] = pages[-1].strip
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
      ha['bibtex_editor'] = ha['editor']
      editor = splitauthors(ha, obj, type=:editor)
      ha.tap { |hs| hs.delete('editor') }
      ha['editor'] = editor
    end
    
    if ha.has_key?('author')
      ha['bibtex_author'] = ha['author']
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
  def self.yamltohash(obj)
  end
  def self.mindigit(str, num=2)
    str.gsub(/-[0-9]+/, '')
    while str.length < num
      str = '0' + str
    end
    return str
  end
  
  def self.filename(date, title)
    puts title
    f = date.to_s + '-' + title.to_s + '.md'
    return f
  end
  
  def self.splitauthors(ha, obj, type=:author)
    puts obj[:author]
    a = Array.new(obj[type].length)       #=> [nil, nil, nil]
    obj[type].each.with_index(0) do |name, index|
      first = detex(name.first)
      last = detex(name.last)
      a[index] = {'given' => first, 'family' => last}
    end
    return a
  end
  def self.disambiguate_chars(count)
    div, mod = count.divmod(26)
    if div == 0
      return (mod + 97).chr
    else
      return disambiguate_chars(div-1) + (mod+97).chr
    end
  end
  def self.extractpapers(bib_file, volume, volume_info, software_file=nil, video_file=nil, supp_file=nil, supp_name=nil)
    # Extract paper info from bib file and put it into yaml files in _posts
    
    # Extract information about software links from a csv file.
    if software_file.nil?
      software_data = nil
    else
      software_data = Hash[*CSV.read(software_file).flatten]
    end

    # Extract information about software links from a csv file.
    if video_file.nil?
      video_data = nil
    else
      video_data = Hash[*CSV.read(video_file).flatten]
    end

    # Extract information about software links from a csv file.
    if supp_file.nil?
      supp_data = nil
    else
      supp_data = Hash[*CSV.read(supp_file).flatten]
    end
    
    
    file = File.open(bib_file, "rb")
    contents = file.read

    bib = BibTeX.parse(contents)
    # do work on files ending in .rb in the desired directory
    ids = []
    bib['@inproceedings'].each do |obj|
      obj.replace(bib.q('@string'))
      obj.join
      ha = bibtohash(obj)
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
      
      ha['address'] = volume_info['address']
      ha['publisher'] = 'PMLR'
      ha['container-title'] = volume_info['booktitle']
      ha['volume'] = volume.to_s
      ha['genre'] = 'inproceedings'
      ha['issued'] = {'date-parts' => [published.year, published.month, published.day]}

      count = 0
      # Fix up the filestubs
      filestub = (ha['author'][0]['family'].downcase + volume_info['published'].strftime('%y') + disambiguate_chars(count)).parameterize
      while ids.include? filestub
        count += 1
        filestub = (ha['author'][0]['family'].downcase + volume_info['published'].strftime('%y') + disambiguate_chars(count)).parameterize
      end
      ids.push(filestub)
      puts filestub
      #puts ha['author'][0]['family'] + published.year.to_s.slice(-2,-1) + 'a'
      #puts ha['id']

      # True for volumes that didn't necessarily conform to original layout
      puts volume.to_i
      inc_layout = ([27..53] + [55..56] + [63..64]).include?(volume.to_i)
      puts inc_layout
      puts 
      # Move all pdfs to correct directory with correct filename
      if inc_layout
        ha['pdf'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + ha['id'] + '.pdf'
      else
        if File.file?(ha['id'] + '.pdf')
          Dir.mkdir(filestub) unless File.exists?(filestub)
          if not File.file?(filestub + '/' + filestub + '.pdf')
            FileUtils.mv(ha['id'] + '.pdf', filestub + '/' + filestub + '.pdf')
          end
        end
        if File.file?(filestub + '/' + filestub + '.pdf')
          ha['pdf'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + filestub + '/' + filestub + '.pdf'
        else
          raise "PDF " + filestub + '/' + filestub + '.pdf' + " file not present"
        end
      end
      
      # Add software link if it is available.
      if not ha.has_key?('software') and not software_data.nil? and software_data.has_key?(ha['id'])
          ha['software'] = software_data[ha['id']]
      end
      # Add video link if it is available.
      if not ha.has_key?('video') and not video_data.nil? and video_data.has_key?(ha['id'])
          ha['video'] = video_data[ha['id']]
      end
      # Move all supplementary files to relevant directory
      Dir.glob(ha['id'] +'-supp.*') do |supp_file|
        newfilename =  supp_file.gsub(ha['id'], filestub)
        Dir.mkdir(filestub) unless File.exists?(filestub)
        if not File.file?(filestub + '/' + newfilename)
          FileUtils.mv(supp_file, filestub + '/' + newfilename)
        end
      end
      if ha.has_key?('supplementary')
        supple = ha['supplementary'].split(':')[-1]
      else
        supple = filestub + '-supp.pdf'
      end

      # Link to all -supp files in directory
      if inc_layout
        ha['supplementary'] = 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + supple
      else
        ha['extras'] = []
        Dir.glob(filestub + '/' + filestub +'-supp.*') do |supp_file|
          ha['extras'] += [{'label' => 'Supplementary ' + File.extname(supp_file)[1..-1].upcase, 'link' => 'http://proceedings.mlr.press' + '/v' + ha['volume'] + '/' + supp_file}]
        end
        # Add supp link if it is available.
        if not supp_data.nil? and supp_data.has_key?(ha['id'])
          if not ha.has_key?('extras')
            ha['extras'] = []
          end
          ha['extras'] += [{'label' => supp_name, 'link'=> supp_data[ha['id']]}]
        end
      end

        
        
      # If it's not in the bad layout then update key
      if not inc_layout
        ha['id'] = filestub
      end
      
      ya = ha.to_yaml(:ExplicitTypes => true)
      fname = filename(published, filestub)
      out = File.open('_posts/' + fname, 'w')
      out.puts ya
      out.puts "# Format based on citeproc: http://blog.martinfenner.org/2013/07/30/citeproc-yaml-for-bibliographies/"
      out.puts "---"
    end  
  end

  def self.extractconfig()
    ha = YAML::load( File.open('_config.yml'))

    return ha
  end

    
  
  def self.bibextractconfig(bibfile, volume)
    # Extract information about the volume from the bib file, place in _config.yml
    file = File.open(bibfile, "rb")
    contents = file.read
    reponame = 'v' + volume.to_s
    bib = BibTeX.parse(contents)
    obj = bib['@proceedings'][0]
    obj.replace(bib.q('@string'))
    obj.join
    ha = bibtohash(obj)
    puts ha
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
    ha['author'] = {'name' => 'PMLR'}
    ha['baseurl'] = '/' + reponame
    ha['twitter_username'] = twitter
    ha['github_username'] = 'mlresearch'
    ha['markdown'] = 'kramdown'
    ha['exclude'] = ['README.md', 'Gemfile', '.gitignore']
    ha['plugins'] = ['jekyll-feed', 'jekyll-seo-tag', 'jekyll-remote-theme']
    ha['remote_theme'] = 'lawrennd/proceedings'
    ha['style'] = 'pmlr'
    ha['permalink'] = '/:title.html'
    ha['ghub'] = {'edit' => true, 'repository' => reponame}
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
    #system "jekyll new " + self.procdir + reponame
    #File.delete(*Dir.glob(self.procdir + reponame + '/_posts/*.markdown'))
    # Add details to _config.yml file
    ha['volume'] = volume.to_i
    ha['email'] = email
    address = detex(ha['address'])
    ha['conference'] = {'name' => ha['name'], 'url' => ha['conference_url'], 'location' => address, 'dates'=>ha['start'].upto(ha['end']).collect{ |i| i}}
    ha.tap { |hs| hs.delete('address') }
    ha.tap { |hs| hs.delete('conference_url') }
    ha.tap { |hs| hs.delete('name') }
    
    ha['analytics'] = {'google' => {'tracking_id' => self.tracking_id}}
    ha['orig_bibfile'] = bibfile
    return ha
  end
  def self.write_volume_files(ha)
    write_config(ha)
    write_index(ha)
    write_readme(ha)
    write_gemfile(ha)
  end  
  def self.write_config(ha)
    ya = ha.to_yaml(:ExplicitTypes => true)

    out = File.open('_config.yml', 'w')    
    out.puts ya
    out.puts "# Site settings"
    out.puts "# Original source:  " + ha['orig_bibfile']
  end
  def self.write_index(ha)
    ind = {'layout' => 'home'}
    indtxt = ind.to_yaml(:ExplicityTypes => true)
    out = File.open('index.html', 'w')
    out.puts indtxt
    out.puts "---"
  end
  def self.write_gemfile(ha)
  
    out = File.open('Gemfile', 'w')
    # frozen_string_literal: true
    out.puts 'source "https://rubygems.org"'
    out.puts
    out.puts 'git_source(:github) {|repo_name| "https://github.com/#{repo_name}" }'
    out.puts
    out.puts 'gem \'jekyll\''
    out.puts
    out.puts 'group :jekyll_plugins do'
    out.puts '  gem \'github-pages\''
    out.puts '  gem \'jekyll-remote-theme\''
    out.puts '  gem \'jekyll-include-cache\''
    out.puts 'end'
    out.puts
    out.puts '# gem "rails"'
    out.puts
  end
  def self.write_readme(ha)

    out = File.open('README.md', 'w')
    readme = ''
    readme += "\n\nPublished as Volume " + ha['volume'].to_s + " by the Proceedings of Machine Learning Research on #{ha['published'].strftime('%d %B %Y')}." + "\n"
    
    if ha.has_key?('editor')
      readme += "\nVolume Edited by:\n"
      for name in ha['editor'] 
        readme += "  * #{name['given']} #{name['family']}\n"
      end
    end
    readme += "\nSeries Editors:\n  * Neil D. Lawrence\n"
    if (ha['volume'].to_i>27)
      readme += "  * Mark Reid\n"
    end
    out.puts '# PMLR V' + ha['volume'].to_s
    out.puts
    out.puts 'To suggest fixes to this volume please make a pull request containng the changes requested and a justificaiton for the changes.'
    out.puts 
    out.puts 'To edit the details of this conference work edit the [_config.yml](./_config.yml) file and submit a pull request.'
    out.puts
    out.puts 'To make changes to the individual paper details, edit the associated paper file in the [./_posts](./_posts) subdirectory.'
    out.puts
    out.puts 'For details of how to publish in PMLR please check http://proceedings.mlr.press/faq.html'
    out.puts
    out.puts 'For details of what is required to submit a proceedings please check http://proceedings.mlr.press/spec.html'
    out.puts
    out.puts readme
    
  end  
end

