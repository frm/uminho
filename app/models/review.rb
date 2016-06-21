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
  validates :score, presence: true, inclusion: { in: 0..10.0 }
  validate  :score_steps_in_halves

  def movie
    Movie.find(movie_id)
  end

  def reliability
    get_upvotes.size - get_downvotes.size
  end

  protected

  def score_steps_in_halves
    unless score and score % 0.5 == 0
      errors.add(:score, "must be multiple of 0.5")
    end
  end
end
