#!/usr/bin/env ruby
# File for updating _config.yml for a new proceedings from a bib file.
# Usage: ./update_config.rb NN
# Where NN is the volume number

require 'rubygems'
require 'bibtex'
require 'yaml'
require 'facets'
require 'latex/decode'
require 'fileutils'
require 'pandoc-ruby'
require_relative 'mlresearch'

if ARGV.length < 1
  puts "Usage: #{$0} <volume>"
else
  volume = ARGV[0]
  reponame = 'v' + volume.to_s

  proceedings = 'v' + volume.to_s + '.bib'
  puts MLResearch.bibdir + proceedings
  bib = BibTeX.open(MLResearch.bibdir + proceedings)
  obj = bib['@proceedings'][0]
  obj.replace(bib.q('@string'))
  obj.join
  ha = MLResearch.bibtohash(obj)
  ha['title'] = MLResearch.publisher
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
    ha['description'] += "\n\nPublished in #{ha['sections'].length} Sections as Volume " + volume.to_s + " by the " + MLResearch.publisher + ".\n"
    ha['sections'].each.with_index(0) do |section, index|
      ha['description'] += "  #{section['title']} published on #{section['published'].strftime('%d %B %Y')}\n"
    end
  else
    ha['description'] += "\n\nPublished as Volume " + volume.to_s + " by the " + MLResearch.publisher + " on #{ha['published'].strftime('%d %B %Y')}." + "\n"
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
  ha['url'] = MLResearch.url
  ha['baseurl'] = '/' + reponame
  ha['twitter_username'] = MLResearch.twitter
  ha['github_username'] = MLResearch.github
  ha['markdown'] = MLResearch.markdown
  ha['permalink'] = '/:title.html'
  ha['github'] = {'edit' => true, 'repository' => reponame}
  if not ha.has_key?('name')
    ha['name'] = booktitle
  end
  ha['display'] = {'copy_button' => {'bibtex' => true, 'endnote' => true, 'apa' => true, 'ris' => true}}
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
  #system "jekyll new " + MLResearch.procdir + reponame
  #File.delete(*Dir.glob(MLResearch.procdir + reponame + '/_posts/*.markdown'))
  # Add details to _config.yml file
  ha['volume'] = volume.to_i
  ha['email'] = MLResearch.email
  address = ha['address']
  
  ha['conference'] = {'name' => ha['name'], 'url' => ha['conference_url'], 'location' => address, 'dates'=>ha['start'].upto(ha['end']).collect{ |i| i}}
  ha.tap { |hs| hs.delete('address') }
  ha.tap { |hs| hs.delete('conference_url') }
  ha.tap { |hs| hs.delete('name') }

  # grab conference number
  if ha.has_key?('conference_number')
    ha['conference']['number'] = ha['conference_number'].to_i
    ha.tap { |hs| hs.delete('conference_number') }
  end
  
  ha['analytics'] = {'google' => {'tracking_id' => MLResearch.tracking_id}}
  ya = ha.to_yaml(:ExplicitTypes => true)

  
  out = File.open('_config.yml', 'w')    
  out.puts ya
  out.puts "# Site settings"
  out.puts "# Auto generated from " + proceedings
  out.puts "---"
end


