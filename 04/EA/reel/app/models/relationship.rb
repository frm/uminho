class Relationship < ActiveRecord::Base
  include PublicActivity::Model
  tracked owner: Proc.new{ |controller, model| controller.current_user },
    only: [:create]
  has_many :activities, as: :trackable,
    class_name: 'PublicActivity::Activity', dependent: :destroy

  belongs_to :follower, class_name: "User"
  belongs_to :followed, class_name: "User"

  validates :follower_id, presence: true
  validates :followed_id, presence: true
end
