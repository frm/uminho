class User < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable, :validatable

  acts_as_voter

  validates :email, presence: true, email: true
  validates :name,  presence: true, length: { maximum: 75 }
  validates :bio,   length: { maximum: 300 }

  has_attached_file :avatar, styles: { medium: "300x300#", thumb: "100x100#"  }, default_url: "http://dummyimage.com/:dimensions/333/eaeaea.png&text=:initials"
  validates_attachment_content_type :avatar, content_type: /\Aimage\/.*\Z/

  validates_confirmation_of :password

  has_many :reviews, dependent: :destroy

  has_many :active_relationships,   class_name:   "Relationship",
                                    foreign_key:  "follower_id",
                                    dependent:    :destroy

  has_many :passive_relationships,  class_name:   "Relationship",
                                    foreign_key:  "followed_id",
                                    dependent:    :destroy

  has_many :following, through: :active_relationships,  source: :followed
  has_many :followers, through: :passive_relationships, source: :follower

  def follow(user)
    active_relationships.create(followed_id: user.id)
  end

  def unfollow(user)
    active_relationships.find_by(followed_id: user.id).destroy
  end

  def following?(user)
    following.include? user
  end

  def followed_by?(user)
    followers.include? user
  end

  def followers_count
    followers.count
  end

  def following_count
    following.count
  end

  def reviews_count
    reviews.count
  end

  def average_rating
    Review.average_rating_for self.reviews
  end

  def current_average_rating
    Review.average_rating_for Review.where(user_id: id).limit(5)
  end

  def monthly_average_rating
    Review.average_rating_for Review.monthly_reviews_for(id)
  end

  def reviewed_movies
    reviews.map(&:movie) # has_many through
  end

  def reliability
    reviews.inject(0) { |sum, review| sum + review.reliability }
  end

  def favorite_genres
    best_reviews = reviews.select { |r| r.score > 3.0 }
    reviewed_genres = {}

    best_reviews.each do |r|
      r.movie.genre_ids.each do |g|
        score = reviewed_genres[g] || 0
        reviewed_genres[g] = score + r.score
      end
    end

    reviewed_genres.sort_by { |genre, score| score }.last(3).map(&:first)
  end

  def reliable_following(n = 1)
    following.sort { |a, b| a.reliability <=> b.reliability }.last(n)
  end

  def tailored_from(other_user)
    fav_genres = favorite_genres
    rev_movies = reviewed_movies

    other_user.reviews.select do |r|
      !(fav_genres & r.movie.genre_ids).empty? && !rev_movies.include?(r.movie)
    end.map(&:movie)
  end

  def review_for(movie)
    Review.find_by(user_id: id, movie_id: movie.id)
  end

  def feed
    PublicActivity::Activity.joins("LEFT OUTER JOIN relationships
                ON relationships.followed_id = activities.owner_id
                WHERE relationships.follower_id=#{id}
                ORDER BY activities.created_at DESC")
  end

  def name_initials
    name.split(' ').map { |n| n[0] }.join('')
  end
end
