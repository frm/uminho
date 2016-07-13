class Actor < ActiveRecord::Base
  has_many :movies

  def self.find(id)
    Actor::Loader.find(id.to_s)
  end

  def movies
    Actor::Loader.find_movies(id.to_s)
  end

  def age
    now = Time.now.utc.to_date
    dob = birthday
    now.year - dob.year - ((now.month > dob.month || (now.month == dob.month && now.day >= dob.day)) ? 0 : 1)
  end
end
