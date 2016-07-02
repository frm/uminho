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
    Movie::Loader.trending
  end

  def self.upcoming
    Movie::Loader.upcoming
  end

  def self.releases
    Movie::Loader.releases
  end

  def self.popular
    Review.order(score: :desc).limit(20).map(&:movie)
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

  def cache_genres(genres)
    @genres = genres
  end

  def genres
    @genres || Movie.find(id).genres # Force a reload
  end

  def genre_ids
    genres.map { |g| g["id"] }
  end

  def genre_names
    genres.map { |g| g["name"] }
  end
end
