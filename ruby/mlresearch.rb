require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'

module MLResearch
  def self.procdir
    '/Users/neil/mlresearch/'
  end
  def self.bibdir
    '/Users/neil/mlresearch/papersite/db/'
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
  class String
    def is_i?
      /\A[-+]?\d+\z/ === self
    end
  end

  def self.detex(text)
    # Returning up to second end character is to deal with new line
    return LaTeX.decode text
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

    if ha.has_key?('abstract')
      if ha['abstract'] == ''
        ha.tap { |hs| hs.delete('abstract') }
      else
        ha['abstract'] = detex(ha['abstract'])
      end
    end
    if ha.has_key?('title')
      ha['tex_title'] = ha['title']
      ha['title'] = detex(ha['title'])
    end
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
    a = Array.new(obj[type].length)       #=> [nil, nil, nil]
    obj[type].each.with_index(0) do |name, index|
      first = detex(name.first)
      last = detex(name.last)
      a[index] = {'given' => first, 'family' => last}
    end
    return a
  end
end
