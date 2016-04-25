class Review < ActiveRecord::Base
  belongs_to :user
  belongs_to :movie

  validates :user_id, presence: true
  validates :movie_id, presence: true, uniqueness: { scope: :user_id,
                                                     message: "review already exists"}
  validates :description, length: { maximum: 1000 }
  validates :score, presence: true, inclusion: { in: 0..10.0 }
  validate  :score_steps_in_halves

  protected

  def score_steps_in_halves
    unless score and score % 0.5 == 0
      errors.add(:score, "must be multiple of 0.5")
    end
  end
end
