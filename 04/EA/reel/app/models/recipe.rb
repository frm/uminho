class Recipe < ActiveRecord::Base
  def self.all
    Recipe::Searcher.all
  end

  def embed_url
    url.split("=").last
  end
end
