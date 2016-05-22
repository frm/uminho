class Actor < ActiveRecord::Base
  has_many :movies

  def self.find(id)
    Actor::Loader.find(id.to_s)
  end

  def movies
  	Actor::Loader.find_movies(id.to_s)
  end
end
