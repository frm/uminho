class Movie < ActiveRecord::Base
  has_many :reviews

  def rating
    self.reviews.average(:score)
  end
end
