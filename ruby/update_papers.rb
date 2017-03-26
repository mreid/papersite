#!/usr/bin/env ruby
#!/usr/bin/env ruby
# File for updating _posts directory with papers for a new proceedings from a directory of bib files.
# Usage: ./update_papers.rb NN
# Where NN is the volume number


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
