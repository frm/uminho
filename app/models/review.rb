class Review < ActiveRecord::Base
  include PublicActivity::Model
  tracked owner: Proc.new{ |controller, model| controller.current_user },
    only: [:create, :update]
  has_many :activities, as: :trackable,
    class_name: 'PublicActivity::Activity', dependent: :destroy

  acts_as_votable

  belongs_to :user
  belongs_to :movie

  validates :user_id, presence: true
  validates :movie_id, presence: true, uniqueness: { scope: :user_id,
                                              message: "review already exists"}
  validates :description, length: { maximum: 1000 }
  validates :score, presence: true, inclusion: { in: 0..5.0 }
  validate  :score_steps_in_halves

  def movie
    Movie.find(movie_id)
  end

  def reliability
    get_upvotes.size - get_downvotes.size
  end

  def self.monthly_reviews_for(user_id)
    Review.where(
      updated_at: (Time.now - 1.month)..(Time.now + 1.month),
      user_id: user_id)
  end

  def self.average_rating_for(reviews)
   (reviews.average(:score) || 0).round(1).to_f
  end

  protected

  def score_steps_in_halves
    unless score and score % 0.5 == 0
      errors.add(:score, "must be multiple of 0.5")
    end
  end
end
