class User < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable, :validatable

  validates :email, presence: true
  validates :name,  presence: true, length: { maximum: 75 }
  validates :bio,   length: { maximum: 300 }

  has_many :reviews
end
