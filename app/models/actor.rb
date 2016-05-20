class Actor < ActiveRecord::Base
  has_many :movies

  def self.find(id)
    Actor::TraktLoader.find(id.to_s)
  end
end
