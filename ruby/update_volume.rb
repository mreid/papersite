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

volume_info = MLResearch.extractconfig()
new_vol_info = {}
volume_info.each do |key, item|
  newkey = key
  if key == 'series'
    if not volume_info.has_key?('publisher')
      new_vol_info['publisher'] = 'PMLR'
    end
  end
  if key == 'github'
    newkey = 'ghub'
  end
  new_vol_info[newkey] = item
  
  if key == 'markdown'
    if not volume_info.has_key?('plugins')
      new_vol_info['plugins'] = ['jekyll-feed', 'jekyll-seo-tag', 'jekyll-remote-theme']
    end
    if not volume_info.has_key?('remote_theme')
      new_vol_info['remote_theme'] = 'lawrennd/proceedings'
    end
    if not volume_info.has_key?('style')
      new_vol_info['style'] = 'pmlr'
    end
    if not volume_info.has_key?('excludes')
      new_vol_info['exclude'] = ['README.md', 'Gemfile', '.gitignore']
    end
  end
  if key == 'series'
    if not volume_info.has_key?('issn')
      if (volume_info['published']).to_time.to_i > (Date.parse '2017-04-09').to_time.to_i
        new_vol_info['issn'] = '2640-3498'
      else
        new_vol_info['issn'] = '1938-7228'
      end
    end
  end
  if key == 'url'
    if not volume_info.has_key?('author')
      new_vol_info['author'] = {'name' => 'PMLR'}
    end
  end
  if not new_vol_info.has_key?('orig_bibfile')
    new_vol_info['orig_bibfile'] = Dir['*.bib'][0]
  end
end
MLResearch.write_volume_files(new_vol_info)
