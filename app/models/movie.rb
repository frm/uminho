class Movie < ActiveRecord::Base
  has_many :reviews

  def rating
    self.reviews.average(:score) || 0
  end

  def ratings
    self.reviews.count
  end

  def self.find(id)
    Movie::Loader.find(id.to_s)
  end

  def self.trending
    Movie::Loader.all
  end

  def cast
    res = Movie::Loader.find_cast(id.to_s) unless @cast
    @directors ||= res[1]
    @cast ||= res[0]
  end

  def directors
    res = Movie::Loader.find_cast(id.to_s) unless @directors
    @cast ||= res[0]
    @directors ||= res[1]
  end
end
