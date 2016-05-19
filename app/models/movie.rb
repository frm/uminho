class Movie < ActiveRecord::Base
  has_many :reviews

  def rating
    self.reviews.average(:score)
  end


  def self.find(id)
    Movie::TraktLoader.find(id.to_s)
  end
end
