class Genre < ActiveRecord::Base
  def movies
    Genre::Loader.movies(id.to_s)
  end

  def self.movies(id)
    Genre::Loader.movies(id.to_s)
  end

  def self.all
    Genre::Loader.all
  end
end
