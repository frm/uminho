class Actor < ActiveRecord::Base
  has_many :movies

  def self.find(id)
    Actor::TraktLoader.find(id.to_s)
  end

  def movies
  	Actor::TraktLoader.find_movies(id.to_s)
  end
end
